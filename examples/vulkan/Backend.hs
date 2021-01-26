{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend where

-- base
import Control.Category
  ( (>>>) )
import Control.Monad
  ( guard, unless, void )
import Data.Bits
  ( (.&.), (.|.) )
import Data.Coerce
  ( coerce )
import Data.Foldable
  ( toList )
import Data.Functor
  ( (<&>) )
import Data.List
  ( sortOn )
import Data.Maybe
  ( fromMaybe )
import Data.Ord
  ( Down(..) )
import Data.Semigroup
  ( First(..) )
import Data.String
  ( fromString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32 )
import Foreign.C.String
  ( CString )
import Foreign.C.Types
  ( CInt )
import Foreign.Ptr
  ( castPtr )

-- bytestring
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString.Short as ShortByteString
  ( packCString )

-- containers
import qualified Data.Map.Strict as Map
  ( empty, insertWith, toList )

-- logging-effect
import Control.Monad.Log
  ( MonadLog, Severity(..), WithSeverity(..)
  , logDebug, logInfo
  )

-- resourcet
import Control.Monad.Trans.Resource
  ( MonadResource )
import qualified Control.Monad.Trans.Resource as ResourceT
  ( ReleaseKey, allocate )

-- sdl2
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( intercalate, pack, fromShortByteString, toByteString, unpack )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO(liftIO) )

-- unliftio-core
import Control.Monad.IO.Unlift
  ( MonadUnliftIO )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( (!?), empty, find, fromList, imap, imapMaybe, singleton, toList )

-- vulkan
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
import qualified Vulkan.Requirement     as Vulkan
import qualified Vulkan.Zero            as Vulkan

-- vulkan-utils
import qualified Vulkan.Utils.Initialization as Vulkan.Utils
  ( createInstanceFromRequirements, createDebugInstanceFromRequirements
  , createDeviceFromRequirements
  )

-- dear-imgui
import Attachments
  ( AttachmentType, SubpassAttachments, SubpassAttachmentReferences
  , attachmentReferencesAndDescriptions
  , createSubpass
  )

--------------------------------------------------------------------------------

type LogMessage = WithSeverity ShortText
class    ( MonadUnliftIO m, MonadResource m, MonadLog LogMessage m ) => MonadVulkan m
instance ( MonadUnliftIO m, MonadResource m, MonadLog LogMessage m ) => MonadVulkan m

----------------------------------------------------------------------------
-- Logging.

logHandler :: MonadIO m => LogMessage -> m ()
logHandler ( WithSeverity sev mess )
  = liftIO . putStrLn . ShortText.unpack $ showSeverity sev <> " " <> mess

showSeverity :: Severity -> ShortText
showSeverity Emergency     = "! PANIC !"
showSeverity Alert         = "! ALERT !"
showSeverity Critical      = "! CRIT !"
showSeverity Error         = "[ERR]  "
showSeverity Warning       = "[WARN] "
showSeverity Notice        = "(note) "
showSeverity Informational = "(info) "
showSeverity Debug         = "(debug)"


data VulkanContext =
  VulkanContext
    { instance'      :: !Vulkan.Instance
    , physicalDevice :: !Vulkan.PhysicalDevice
    , device         :: !Vulkan.Device
    , queueFamily    :: !Word32
    , queue          :: !Vulkan.Queue
    }

data InstanceType
  = NormalInstance
  | DebugInstance
  deriving stock Show

data VulkanRequirements =
  VulkanRequirements
    { instanceRequirements :: [ Vulkan.InstanceRequirement ]
    , deviceRequirements   :: [ Vulkan.DeviceRequirement   ]
    , queueFlags           :: Vulkan.QueueFlags
    }

data ValidationLayerName
  = LunarG
  | Khronos
  deriving stock ( Eq, Show )

initialiseVulkanContext :: MonadVulkan m => InstanceType -> ByteString -> VulkanRequirements -> m VulkanContext
initialiseVulkanContext instanceType appName ( VulkanRequirements { instanceRequirements, deviceRequirements, queueFlags } ) = do
  logDebug "Creating Vulkan instance"
  instanceInfo    <- vulkanInstanceInfo appName
  instance'       <- case instanceType of
    NormalInstance -> Vulkan.Utils.createInstanceFromRequirements      instanceRequirements [] instanceInfo
    DebugInstance  -> Vulkan.Utils.createDebugInstanceFromRequirements instanceRequirements [] instanceInfo
  physicalDevice  <- logDebug "Creating physical device"      *> createPhysicalDevice instance'
  queueFamily     <- logDebug "Finding suitable queue family" *> findQueueFamilyIndex physicalDevice queueFlags
  let
    queueCreateInfo :: Vulkan.DeviceQueueCreateInfo '[]
    queueCreateInfo = Vulkan.zero
      { Vulkan.queueFamilyIndex = fromIntegral queueFamily
      , Vulkan.queuePriorities  = Boxed.Vector.singleton ( 1.0 :: Float )
      }
    deviceCreateInfo :: Vulkan.DeviceCreateInfo '[]
    deviceCreateInfo = Vulkan.zero { Vulkan.queueCreateInfos = Boxed.Vector.singleton ( Vulkan.SomeStruct queueCreateInfo ) }
    swapchainDeviceRequirements :: [ Vulkan.DeviceRequirement ]
    swapchainDeviceRequirements
      = Vulkan.RequireDeviceExtension Nothing Vulkan.KHR_SWAPCHAIN_EXTENSION_NAME 0
      : deviceRequirements
  device <- logDebug "Creating logical device" *>
    Vulkan.Utils.createDeviceFromRequirements swapchainDeviceRequirements [] physicalDevice deviceCreateInfo
  queue  <- Vulkan.getDeviceQueue device ( fromIntegral queueFamily ) 0
  
  pure ( VulkanContext { .. } )
  


vulkanInstanceInfo
  :: MonadVulkan m
  => ByteString
  -> m ( Vulkan.InstanceCreateInfo '[] )
vulkanInstanceInfo appName = do

  ( availableLayers :: Boxed.Vector Vulkan.LayerProperties ) <- snd <$> Vulkan.enumerateInstanceLayerProperties

  let
    validationLayer :: Maybe ValidationLayerName
    validationLayer
      = coerce 
      . foldMap
        (  (  Vulkan.layerName :: Vulkan.LayerProperties -> ByteString )
        >>> \case
              "VK_LAYER_LUNARG_standard_validation" -> Just ( First LunarG  )
              "VK_LAYER_KHRONOS_validation"         -> Just ( First Khronos )
              _                                     -> Nothing
        )
      $ availableLayers

    enabledLayers :: [ ByteString ]
    enabledLayers = case validationLayer of
      Nothing      -> []
      Just LunarG  -> [ "VK_LAYER_LUNARG_standard_validation" ]
      Just Khronos -> [ "VK_LAYER_KHRONOS_validation" ]

    appInfo :: Vulkan.ApplicationInfo
    appInfo =
      Vulkan.ApplicationInfo
        { Vulkan.applicationName    = Just appName
        , Vulkan.applicationVersion = 0
        , Vulkan.engineName         = Nothing
        , Vulkan.engineVersion      = 0
        , Vulkan.apiVersion         = Vulkan.API_VERSION_1_2
        }

    createInfo :: Vulkan.InstanceCreateInfo '[]
    createInfo =
      Vulkan.InstanceCreateInfo
        { Vulkan.next                  = ()
        , Vulkan.flags                 = Vulkan.zero
        , Vulkan.applicationInfo       = Just appInfo
        , Vulkan.enabledLayerNames     = Boxed.Vector.fromList enabledLayers
        , Vulkan.enabledExtensionNames = mempty
        }

  case validationLayer of
    Nothing -> logInfo "Validation layer unavailable. Is the Vulkan SDK installed?"
    Just _  -> logInfo ( "Enabled validation layers " <> ShortText.pack ( show enabledLayers ) )

  pure createInfo

createPhysicalDevice :: MonadVulkan m => Vulkan.Instance -> m Vulkan.PhysicalDevice
createPhysicalDevice vk = do
  physicalDevices <- snd <$> Vulkan.enumeratePhysicalDevices vk

  typedDevices <-
    for physicalDevices \ physicalDevice -> do
      properties <- Vulkan.getPhysicalDeviceProperties physicalDevice
      pure ( physicalDevice, Vulkan.deviceType properties )

  case Boxed.Vector.find ( isSuitableDeviceType . snd ) typedDevices of
    Nothing       -> error "Could not find a suitable physical device"
    Just ( d, _ ) -> pure d

  where
    isSuitableDeviceType :: Vulkan.PhysicalDeviceType -> Bool
    isSuitableDeviceType
      = flip elem
          [ Vulkan.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          , Vulkan.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          ]

findQueueFamilyIndex
  :: MonadIO m
  => Vulkan.PhysicalDevice
  -> Vulkan.QueueFlags
  -> m Word32
findQueueFamilyIndex physicalDevice requiredFlags = do
  queueFamilies <- Vulkan.getPhysicalDeviceQueueFamilyProperties physicalDevice
  let
    capableFamilyIndices :: Boxed.Vector Int
    capableFamilyIndices = ( `Boxed.Vector.imapMaybe` queueFamilies ) \ i queueFamily -> do
      guard ( Vulkan.queueFlags queueFamily .&. requiredFlags > Vulkan.zero )
      pure i
  case capableFamilyIndices Boxed.Vector.!? 0 of
    Nothing -> error "No queue family has sufficient capabilities"
    Just i  -> pure ( fromIntegral i )

instanceExtensions :: [ ByteString ] -> [ Vulkan.InstanceRequirement ]
instanceExtensions = map mkExtensionRequirement
  where
    mkExtensionRequirement :: ByteString -> Vulkan.InstanceRequirement
    mkExtensionRequirement extName =
      Vulkan.RequireInstanceExtension
        { Vulkan.instanceExtensionLayerName  = Nothing
        , Vulkan.instanceExtensionName       = extName
        , Vulkan.instanceExtensionMinVersion = 0
        }


initialiseWindow :: MonadVulkan m => WindowInfo -> m ( SDL.Window, [ ByteString ] )
initialiseWindow ( WindowInfo { height, width, windowName, mouseMode } ) = do
  logDebug "Initializing SDL"
  SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE
  SDL.initialize [ SDL.InitVideo ]
  void ( SDL.setMouseLocationMode mouseMode )
  window           <- logDebug "Creating SDL window"           *> createWindow width height windowName
  neededExtensions <- logDebug "Loading needed extensions"     *> SDL.Video.Vulkan.vkGetInstanceExtensions window
  extensionNames   <- traverse ( liftIO . peekCString ) neededExtensions
  logInfo $ "Needed instance extensions are: " <> ShortText.intercalate ", " extensionNames
  pure ( window, map ShortText.toByteString extensionNames )

peekCString :: CString -> IO ShortText
peekCString = fmap ( fromMaybe "???" . ShortText.fromShortByteString ) . ShortByteString.packCString

data WindowInfo
  = WindowInfo
  { width      :: CInt
  , height     :: CInt
  , windowName :: ShortText
  , mouseMode  :: SDL.LocationMode
  }

createWindow :: MonadVulkan m => CInt -> CInt -> ShortText -> m SDL.Window
createWindow x y title =
  snd <$> ResourceT.allocate
    ( SDL.createWindow
              ( fromString ( ShortText.unpack title ) )
              SDL.defaultWindow
                { SDL.windowGraphicsContext = SDL.VulkanContext
                , SDL.windowInitialSize     = SDL.V2 x y
                , SDL.windowResizable       = True
                }
    )
    SDL.destroyWindow

createSurface
  :: MonadVulkan m
  => SDL.Window
  -> Vulkan.Instance
  -> m SDL.Video.Vulkan.VkSurfaceKHR
createSurface window vulkanInstance =
  snd <$> ResourceT.allocate
    ( SDL.Video.Vulkan.vkCreateSurface window ( castPtr $ Vulkan.instanceHandle vulkanInstance ) )
    ( \ surf -> Vulkan.destroySurfaceKHR vulkanInstance ( Vulkan.SurfaceKHR surf ) Nothing )

assertSurfacePresentable
  :: MonadIO m
  => Vulkan.PhysicalDevice
  -> Word32
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = do
  isPresentable <-
    Vulkan.getPhysicalDeviceSurfaceSupportKHR
      physicalDevice
      queueFamilyIndex
      ( Vulkan.SurfaceKHR surface )

  unless isPresentable ( error "Surface is not presentable" )


chooseSwapchainFormat
  :: MonadIO m
  => Vulkan.SurfaceFormatKHR
  -> Vulkan.PhysicalDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m Vulkan.SurfaceFormatKHR
chooseSwapchainFormat
  preferredFormat@( Vulkan.SurfaceFormatKHR fmt_p spc_p )
  physicalDevice
  surface
  = do
      surfaceFormats <- snd <$> Vulkan.getPhysicalDeviceSurfaceFormatsKHR physicalDevice ( Vulkan.SurfaceKHR surface )

      case sortOn ( Down . score ) ( Boxed.Vector.toList surfaceFormats ) of
        [] -> error "No formats found."
        ( best : _ )
          | Vulkan.FORMAT_UNDEFINED <- ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) best
            -> pure preferredFormat
          | otherwise
            -> pure best

    where
      match :: Eq a => a -> a -> Int
      match a b
        | a == b    = 1
        | otherwise = 0

      score :: Vulkan.SurfaceFormatKHR -> Int
      score ( Vulkan.SurfaceFormatKHR fmt spc )
        = match fmt fmt_p
        + match spc spc_p

createSwapchain
  :: ( MonadIO m, MonadVulkan m )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> Vulkan.SurfaceFormatKHR
  -> Vulkan.ImageUsageFlags
  -> Word32
  -> Maybe Vulkan.SwapchainKHR
  -> m ( ResourceT.ReleaseKey, Vulkan.SwapchainKHR, Vulkan.Extent2D )
createSwapchain physicalDevice device surface surfaceFormat imageUsage imageCount oldSwapchain = do

  surfaceCapabilities <- Vulkan.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice ( Vulkan.SurfaceKHR surface )

  ( _, presentModes ) <- Vulkan.getPhysicalDeviceSurfacePresentModesKHR physicalDevice ( Vulkan.SurfaceKHR surface )
  
  let
    presentMode :: Vulkan.PresentModeKHR
    presentMode 
      | Vulkan.PRESENT_MODE_MAILBOX_KHR `elem` presentModes
      = Vulkan.PRESENT_MODE_MAILBOX_KHR
      | otherwise
      = Vulkan.PRESENT_MODE_FIFO_KHR

    currentExtent :: Vulkan.Extent2D
    currentExtent = ( Vulkan.currentExtent :: Vulkan.SurfaceCapabilitiesKHR -> Vulkan.Extent2D ) surfaceCapabilities

    currentTransform :: Vulkan.SurfaceTransformFlagBitsKHR
    currentTransform = ( Vulkan.currentTransform :: Vulkan.SurfaceCapabilitiesKHR -> Vulkan.SurfaceTransformFlagBitsKHR ) surfaceCapabilities

    swapchainCreateInfo :: Vulkan.SwapchainCreateInfoKHR '[]
    swapchainCreateInfo =
      Vulkan.SwapchainCreateInfoKHR
        { Vulkan.next                  = ()
        , Vulkan.flags                 = Vulkan.zero
        , Vulkan.surface               = Vulkan.SurfaceKHR surface
        , Vulkan.minImageCount         = imageCount
        , Vulkan.imageFormat           = ( Vulkan.format     :: Vulkan.SurfaceFormatKHR -> Vulkan.Format        ) surfaceFormat
        , Vulkan.imageColorSpace       = ( Vulkan.colorSpace :: Vulkan.SurfaceFormatKHR -> Vulkan.ColorSpaceKHR ) surfaceFormat
        , Vulkan.imageExtent           = currentExtent
        , Vulkan.imageArrayLayers      = 1
        , Vulkan.imageUsage            = imageUsage
        , Vulkan.imageSharingMode      = Vulkan.SHARING_MODE_EXCLUSIVE
        , Vulkan.queueFamilyIndices    = Boxed.Vector.empty
        , Vulkan.preTransform          = currentTransform
        , Vulkan.compositeAlpha        = Vulkan.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , Vulkan.presentMode           = presentMode
        , Vulkan.clipped               = True
        , Vulkan.oldSwapchain          = fromMaybe Vulkan.NULL_HANDLE oldSwapchain
        }

  ( key, swapchain ) <- Vulkan.withSwapchainKHR device swapchainCreateInfo Nothing ResourceT.allocate
  pure ( key, swapchain, currentExtent )


simpleRenderPass
  :: MonadVulkan m
  => Vulkan.Device
  -> SubpassAttachments ( Vulkan.AttachmentDescription, AttachmentType )
  -> m ( ResourceT.ReleaseKey, Vulkan.RenderPass )
simpleRenderPass dev attachments = Vulkan.withRenderPass dev createInfo Nothing ResourceT.allocate
  where

    attachmentReferences   :: SubpassAttachmentReferences
    attachmentDescriptions :: [ Vulkan.AttachmentDescription ]
    ( attachmentReferences, attachmentDescriptions )
      = attachmentReferencesAndDescriptions attachments

    subpass :: Vulkan.SubpassDescription
    subpass = createSubpass attachmentReferences

    dependency1 :: Vulkan.SubpassDependency
    dependency1 =
      Vulkan.SubpassDependency
        { Vulkan.srcSubpass      = Vulkan.SUBPASS_EXTERNAL
        , Vulkan.dstSubpass      = Vulkan.zero
        , Vulkan.srcStageMask    = Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vulkan.srcAccessMask   = Vulkan.zero
        , Vulkan.dstStageMask    = Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vulkan.dstAccessMask   = Vulkan.ACCESS_COLOR_ATTACHMENT_READ_BIT
                               .|. Vulkan.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        , Vulkan.dependencyFlags = Vulkan.zero
        }

    dependency2 :: Vulkan.SubpassDependency
    dependency2 =
      Vulkan.SubpassDependency
        { Vulkan.srcSubpass      = Vulkan.zero
        , Vulkan.dstSubpass      = Vulkan.SUBPASS_EXTERNAL
        , Vulkan.srcStageMask    = Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vulkan.srcAccessMask   = Vulkan.ACCESS_COLOR_ATTACHMENT_READ_BIT
                               .|. Vulkan.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        , Vulkan.dstStageMask    = Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        , Vulkan.dstAccessMask   = Vulkan.zero
        , Vulkan.dependencyFlags = Vulkan.zero
        }

    createInfo :: Vulkan.RenderPassCreateInfo '[]
    createInfo =
      Vulkan.RenderPassCreateInfo
        { Vulkan.next         = ()
        , Vulkan.flags        = Vulkan.zero
        , Vulkan.attachments  = Boxed.Vector.fromList attachmentDescriptions
        , Vulkan.subpasses    = Boxed.Vector.singleton subpass 
        , Vulkan.dependencies = Boxed.Vector.fromList [ dependency1, dependency2 ]
        }

cmdBeginRenderPass
  :: MonadIO m
  => Vulkan.CommandBuffer
  -> Vulkan.RenderPass
  -> Vulkan.Framebuffer
  -> [ Vulkan.ClearValue ]
  -> Vulkan.Extent2D
  -> m ()
cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent =
  let
    zeroZero :: Vulkan.Offset2D
    zeroZero =
      Vulkan.Offset2D
        { Vulkan.x = 0
        , Vulkan.y = 0
        }

    renderArea :: Vulkan.Rect2D
    renderArea =
      Vulkan.Rect2D
        { Vulkan.offset = zeroZero
        , Vulkan.extent = extent
        }

    beginInfo :: Vulkan.RenderPassBeginInfo '[]
    beginInfo =
      Vulkan.RenderPassBeginInfo
        { Vulkan.next        = ()
        , Vulkan.renderPass  = renderPass
        , Vulkan.framebuffer = framebuffer
        , Vulkan.renderArea  = renderArea
        , Vulkan.clearValues = Boxed.Vector.fromList clearValues
        }
  in
    Vulkan.cmdBeginRenderPass
      commandBuffer
      beginInfo
      Vulkan.SUBPASS_CONTENTS_INLINE

cmdNextSubpass :: MonadIO m => Vulkan.CommandBuffer -> m ()
cmdNextSubpass commandBuffer = Vulkan.cmdNextSubpass commandBuffer Vulkan.SUBPASS_CONTENTS_INLINE

cmdEndRenderPass :: MonadIO m => Vulkan.CommandBuffer -> m ()
cmdEndRenderPass = Vulkan.cmdEndRenderPass

createImageView
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.Image
  -> Vulkan.ImageViewType
  -> Vulkan.Format
  -> Vulkan.ImageAspectFlags
  -> m ( ResourceT.ReleaseKey, Vulkan.ImageView )
createImageView dev image viewType fmt aspect = Vulkan.withImageView dev createInfo Nothing ResourceT.allocate
  where
    components :: Vulkan.ComponentMapping
    components =
      Vulkan.ComponentMapping
        { Vulkan.r = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        , Vulkan.g = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        , Vulkan.b = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        , Vulkan.a = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        }

    subResourceRange :: Vulkan.ImageSubresourceRange
    subResourceRange =
      Vulkan.ImageSubresourceRange
        { Vulkan.aspectMask     = aspect
        , Vulkan.baseMipLevel   = 0
        , Vulkan.levelCount     = 1
        , Vulkan.baseArrayLayer = 0
        , Vulkan.layerCount     = 1
        }

    createInfo :: Vulkan.ImageViewCreateInfo '[]
    createInfo =
      Vulkan.ImageViewCreateInfo
        { Vulkan.next             = ()
        , Vulkan.flags            = Vulkan.zero
        , Vulkan.image            = image
        , Vulkan.viewType         = viewType
        , Vulkan.format           = fmt
        , Vulkan.components       = components
        , Vulkan.subresourceRange = subResourceRange
        }

createFramebuffer
  :: ( MonadVulkan m, Foldable f )
  => Vulkan.Device
  -> Vulkan.RenderPass
  -> Vulkan.Extent2D
  -> f Vulkan.ImageView
  -> m ( ResourceT.ReleaseKey, Vulkan.Framebuffer )
createFramebuffer dev renderPass extent attachments = Vulkan.withFramebuffer dev createInfo Nothing ResourceT.allocate
  where
    createInfo :: Vulkan.FramebufferCreateInfo '[]
    createInfo =
      Vulkan.FramebufferCreateInfo
        { Vulkan.next        = ()
        , Vulkan.flags       = Vulkan.zero
        , Vulkan.renderPass  = renderPass
        , Vulkan.attachments = Boxed.Vector.fromList . toList $ attachments
        , Vulkan.width       = ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) extent
        , Vulkan.height      = ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) extent
        , Vulkan.layers      = 1
        }

createDescriptorPool
  :: MonadVulkan m
  => Vulkan.Device
  -> Int
  -> [ ( Vulkan.DescriptorType, Int ) ]
  -> m ( ResourceT.ReleaseKey, Vulkan.DescriptorPool )
createDescriptorPool device maxSets descTypes = Vulkan.withDescriptorPool device createInfo Nothing ResourceT.allocate

    where
      poolSizes :: [ Vulkan.DescriptorPoolSize ]
      poolSizes =
        counts descTypes <&> \ ( descType, descCount ) ->
          Vulkan.DescriptorPoolSize
          { Vulkan.type'           = descType
          , Vulkan.descriptorCount = fromIntegral $ maxSets * descCount
          }
      createInfo :: Vulkan.DescriptorPoolCreateInfo '[]
      createInfo =
        Vulkan.DescriptorPoolCreateInfo
          { Vulkan.next      = ()
          , Vulkan.flags     = Vulkan.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
          , Vulkan.poolSizes = Boxed.Vector.fromList poolSizes
          , Vulkan.maxSets   = fromIntegral maxSets
          }

counts :: ( Ord a, Num i ) => [ ( a, i ) ] -> [ ( a, i ) ]
counts = Map.toList . foldr ( uncurry $ Map.insertWith (+) ) Map.empty

createDescriptorSetLayout
  :: MonadVulkan m
  => Vulkan.Device
  -> [ ( Vulkan.DescriptorType, Vulkan.ShaderStageFlags ) ]
  -> m ( ResourceT.ReleaseKey, Vulkan.DescriptorSetLayout )
createDescriptorSetLayout device descriptorTypes = Vulkan.withDescriptorSetLayout device createInfo Nothing ResourceT.allocate

      where
        bindings :: Boxed.Vector Vulkan.DescriptorSetLayoutBinding
        bindings = ( `Boxed.Vector.imap` Boxed.Vector.fromList descriptorTypes ) \ i ( descType, descStageFlags ) ->
          Vulkan.DescriptorSetLayoutBinding
            { Vulkan.binding           = fromIntegral i
            , Vulkan.descriptorType    = descType
            , Vulkan.descriptorCount   = 1
            , Vulkan.stageFlags        = descStageFlags
            , Vulkan.immutableSamplers = Boxed.Vector.empty
            }
        createInfo :: Vulkan.DescriptorSetLayoutCreateInfo '[]
        createInfo =
          Vulkan.DescriptorSetLayoutCreateInfo
            { Vulkan.next     = ()
            , Vulkan.flags    = Vulkan.zero
            , Vulkan.bindings = bindings
            }

createCommandPool
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPoolCreateFlagBits
  -> Word32
  -> m Vulkan.CommandPool
createCommandPool dev flags queueFamilyIndex = snd <$> Vulkan.withCommandPool dev createInfo Nothing ResourceT.allocate
  where
    createInfo :: Vulkan.CommandPoolCreateInfo
    createInfo =
      Vulkan.CommandPoolCreateInfo
        { Vulkan.flags            = flags
        , Vulkan.queueFamilyIndex = queueFamilyIndex
        }

allocatePrimaryCommandBuffers
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPool
  -> Word32
  -> m ( ResourceT.ReleaseKey, Boxed.Vector Vulkan.CommandBuffer )
allocatePrimaryCommandBuffers dev commandPool count = Vulkan.withCommandBuffers dev allocInfo ResourceT.allocate
    where
      allocInfo :: Vulkan.CommandBufferAllocateInfo
      allocInfo =
        Vulkan.CommandBufferAllocateInfo
          { Vulkan.commandPool        = commandPool
          , Vulkan.level              = Vulkan.COMMAND_BUFFER_LEVEL_PRIMARY
          , Vulkan.commandBufferCount = count
          }

submitCommandBuffer
  :: MonadIO m
  => Vulkan.Queue
  -> Vulkan.CommandBuffer
  -> [ ( Vulkan.Semaphore, Vulkan.PipelineStageFlags ) ]
  -> [ Vulkan.Semaphore ]
  -> Maybe Vulkan.Fence
  -> m ()
submitCommandBuffer queue commandBuffer wait signal mbFence =
  Vulkan.queueSubmit queue ( Boxed.Vector.singleton $ Vulkan.SomeStruct submitInfo ) ( fromMaybe Vulkan.NULL_HANDLE mbFence )
    where
      submitInfo :: Vulkan.SubmitInfo '[]
      submitInfo =
        Vulkan.SubmitInfo
          { Vulkan.next             = ()
          , Vulkan.waitSemaphores   = Boxed.Vector.fromList $ map fst wait
          , Vulkan.waitDstStageMask = Boxed.Vector.fromList $ map snd wait
          , Vulkan.commandBuffers   = Boxed.Vector.singleton ( Vulkan.commandBufferHandle commandBuffer )
          , Vulkan.signalSemaphores = Boxed.Vector.fromList signal
          }

beginCommandBuffer :: MonadIO m => Vulkan.CommandBuffer -> m ()
beginCommandBuffer commandBuffer = Vulkan.beginCommandBuffer commandBuffer commandBufferBeginInfo
  where
    commandBufferBeginInfo :: Vulkan.CommandBufferBeginInfo '[]
    commandBufferBeginInfo =
      Vulkan.CommandBufferBeginInfo
        { Vulkan.next            = ()
        , Vulkan.flags           = Vulkan.zero
        , Vulkan.inheritanceInfo = Nothing
        }

endCommandBuffer :: MonadIO m => Vulkan.CommandBuffer -> m ()
endCommandBuffer = Vulkan.endCommandBuffer

createFence :: MonadVulkan m => Vulkan.Device -> m ( ResourceT.ReleaseKey, Vulkan.Fence )
createFence device = Vulkan.withFence device fenceCreateInfo Nothing ResourceT.allocate
  where
    fenceCreateInfo :: Vulkan.FenceCreateInfo '[]
    fenceCreateInfo =
      Vulkan.FenceCreateInfo
        { Vulkan.next  = ()
        , Vulkan.flags = Vulkan.zero
        }

data Wait a = WaitAll [a] | WaitAny [a]

waitForFences :: MonadIO m => Vulkan.Device -> Wait Vulkan.Fence -> m ()
waitForFences device fences = void $ Vulkan.waitForFences device ( Boxed.Vector.fromList fenceList ) waitAll maxBound
  where
    waitAll   :: Bool
    fenceList :: [Vulkan.Fence]
    (waitAll, fenceList) =
      case fences of
        WaitAll l -> ( True , l )
        WaitAny l -> ( False, l )

createPipelineLayout
  :: MonadVulkan m
  => Vulkan.Device
  -> [ Vulkan.DescriptorSetLayout ]
  -> [ Vulkan.PushConstantRange ]
  -> m ( ResourceT.ReleaseKey, Vulkan.PipelineLayout )
createPipelineLayout device layouts ranges =
  Vulkan.withPipelineLayout device pipelineLayoutCreateInfo Nothing ResourceT.allocate
    where
      pipelineLayoutCreateInfo :: Vulkan.PipelineLayoutCreateInfo
      pipelineLayoutCreateInfo =
        Vulkan.PipelineLayoutCreateInfo
          { Vulkan.flags              = Vulkan.zero
          , Vulkan.setLayouts         = Boxed.Vector.fromList layouts
          , Vulkan.pushConstantRanges = Boxed.Vector.fromList ranges
          }

present
  :: MonadIO m
  => Vulkan.Queue
  -> Vulkan.SwapchainKHR
  -> Word32
  -> [Vulkan.Semaphore]
  -> m Vulkan.Result
present queue swapchain imageIndex wait = Vulkan.queuePresentKHR queue presentInfo
  where
    presentInfo :: Vulkan.PresentInfoKHR '[]
    presentInfo =
      Vulkan.PresentInfoKHR
        { Vulkan.next           = ()
        , Vulkan.waitSemaphores = Boxed.Vector.fromList wait
        , Vulkan.swapchains     = Boxed.Vector.singleton swapchain
        , Vulkan.imageIndices   = Boxed.Vector.singleton imageIndex
        , Vulkan.results        = Vulkan.zero
        }
