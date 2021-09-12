{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

module Main where

-- base
import Control.Arrow
  ( second )
import Control.Exception
  ( throw )
import Control.Monad
  ( unless, void )
import Data.Foldable
  ( traverse_ )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32 )

-- logging-effect
import Control.Monad.Log
  ( LoggingT(..), logDebug, runLoggingT )

-- resource-t
import Control.Monad.Trans.Resource
  ( ResourceT, MonadResource, runResourceT )
import qualified Control.Monad.Trans.Resource as ResourceT
  ( allocate, release )

-- sdl
import qualified SDL

-- transformers
import Control.Monad.Trans.Reader
  ( ReaderT(..) )
import Control.Monad.IO.Class
  ( MonadIO(..) )

-- unliftio
import UnliftIO.Exception
  ( handleJust )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( (!), head, singleton, unzip )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Exception as Vulkan
import qualified Vulkan.Zero      as Vulkan

-- dear-imgui
import Attachments
import Backend
import Input
import qualified DearImGui            as ImGui
import qualified DearImGui.Vulkan     as ImGui.Vulkan
import qualified DearImGui.SDL        as ImGui.SDL
import qualified DearImGui.SDL.Vulkan as ImGui.SDL.Vulkan

--------------------------------------------------------------------------------

type Handler    = LogMessage -> ResourceT IO ()
deriving via ( ReaderT Handler (ResourceT IO) )
  instance MonadResource ( LoggingT LogMessage (ResourceT IO) )

gui :: MonadIO m => m ImGui.DrawData
gui = do
  -- Prepare frame
  ImGui.Vulkan.vulkanNewFrame
  ImGui.SDL.sdl2NewFrame
  ImGui.newFrame

  -- Run your windows
  ImGui.showDemoWindow

  -- Process ImGui state into draw commands
  ImGui.render
  ImGui.getDrawData

main :: IO ()
main = runResourceT . ( `runLoggingT` logHandler ) $ app @( LoggingT LogMessage ( ResourceT IO ) )

appName :: IsString a => a
appName = "DearImGui - Vulkan"

app :: forall m. MonadVulkan m => m ()
app = do

  -------------------------------------------
  -- Initialise window, Vulkan and Dear ImGui contexts.

  ( window, windowExtensions ) <-
    initialiseWindow
      WindowInfo
        { width      = 1280
        , height     = 720
        , windowName = appName
        , mouseMode  = SDL.AbsoluteLocation
        }
  let
    vulkanReqs :: VulkanRequirements
    vulkanReqs =
      VulkanRequirements
        { instanceRequirements = instanceExtensions windowExtensions
        , deviceRequirements   = []
        , queueFlags           = Vulkan.QUEUE_GRAPHICS_BIT
        }
  VulkanContext {..} <- initialiseVulkanContext NormalInstance appName vulkanReqs

  surface <- logDebug "Creating SDL surface" *> createSurface window instance'
  assertSurfacePresentable physicalDevice queueFamily surface

  void $ ResourceT.allocate
    ImGui.createContext
    ImGui.destroyContext

  logDebug "Adding fonts"
  ImGui.clearFontAtlas
  _default <- ImGui.addFontDefault
  _custom <- ImGui.addFontFromFileTTF "imgui/misc/fonts/ProggyTiny.ttf" 10
  ImGui.buildFontAtlas

  let
    preferredFormat :: Vulkan.SurfaceFormatKHR
    preferredFormat =
      Vulkan.SurfaceFormatKHR
        Vulkan.FORMAT_B8G8R8A8_UNORM
        Vulkan.COLOR_SPACE_SRGB_NONLINEAR_KHR
    surfaceUsage :: Vulkan.ImageUsageFlagBits
    surfaceUsage = Vulkan.IMAGE_USAGE_COLOR_ATTACHMENT_BIT

  commandPool  <- createCommandPool device Vulkan.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT queueFamily
  nextImageSem <- snd <$> Vulkan.withSemaphore device Vulkan.zero Nothing ResourceT.allocate
  submitted    <- snd <$> Vulkan.withSemaphore device Vulkan.zero Nothing ResourceT.allocate

  let
    imGuiDescriptorTypes :: [ ( Vulkan.DescriptorType, Int ) ]
    imGuiDescriptorTypes = map (, 1000)
      [ Vulkan.DESCRIPTOR_TYPE_SAMPLER
      , Vulkan.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , Vulkan.DESCRIPTOR_TYPE_SAMPLED_IMAGE
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_IMAGE
      , Vulkan.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
      , Vulkan.DESCRIPTOR_TYPE_INPUT_ATTACHMENT
      ]

  ( _imGuiPoolKey, imGuiDescriptorPool ) <- createDescriptorPool device 1000 imGuiDescriptorTypes

  ---------------------------------------------------------------------------
  -- Handle swapchain creation (and resources that depend on the swapchain).

  surfaceCapabilities <- Vulkan.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice ( Vulkan.SurfaceKHR surface )

  let
    minImageCount, maxImageCount, imageCount :: Word32
    minImageCount = ( Vulkan.minImageCount :: Vulkan.SurfaceCapabilitiesKHR -> Word32 ) surfaceCapabilities
    maxImageCount = ( Vulkan.maxImageCount :: Vulkan.SurfaceCapabilitiesKHR -> Word32 ) surfaceCapabilities
    imageCount
      | maxImageCount == 0 =   minImageCount + 1
      | otherwise          = ( minImageCount + 1 ) `min` maxImageCount

    clearValues :: [ Vulkan.ClearValue ]
    clearValues = [ Vulkan.Color $ Vulkan.Float32 0.5 0.2 0 1.0 ]

    swapchainResources :: Maybe SwapchainResources -> m ( m (), SwapchainResources )
    swapchainResources mbOldResources = do
      ( surfaceFormat, imGuiRenderPass ) <- case mbOldResources of
        Nothing -> do
          logDebug "Choosing swapchain format & color space"
          surfaceFormat <- chooseSwapchainFormat preferredFormat physicalDevice surface
          let
            colFmt :: Vulkan.Format
            colFmt = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormat
          logDebug "Creating Dear ImGui render pass"
          ( _, imGuiRenderPass ) <-
            simpleRenderPass device
              ( noAttachments
                { colorAttachments = Boxed.Vector.singleton $ presentableColorAttachmentDescription colFmt }
              )
          pure ( surfaceFormat, imGuiRenderPass )
        Just oldResources -> pure ( surfaceFormat oldResources, imGuiRenderPass oldResources )

      let
        colFmt :: Vulkan.Format
        colFmt = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormat

      logDebug "Creating swapchain"
      ( swapchainKey, swapchain, swapchainExtent ) <-
        createSwapchain
          physicalDevice device
          surface surfaceFormat
          surfaceUsage
          imageCount
          ( swapchain <$> mbOldResources )

      logDebug "Getting swapchain images"
      swapchainImages <- snd <$> Vulkan.getSwapchainImagesKHR device swapchain

      -------------------------------------------
      -- Create framebuffer attachments.

{-
      let
        width, height :: Num a => a
        width  = fromIntegral $ ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) swapchainExtent
        height = fromIntegral $ ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) swapchainExtent

        extent3D :: Vulkan.Extent3D
        extent3D
          = Vulkan.Extent3D
              { Vulkan.width  = width
              , Vulkan.height = height
              , Vulkan.depth  = 1
              }
-}

      logDebug "Creating framebuffers"
      ( fbKeys, framebuffersWithAttachments ) <-
          fmap Boxed.Vector.unzip . for swapchainImages $ \ swapchainImage -> do
              ( imageViewKey, colorImageView )
                <- createImageView
                      device swapchainImage
                      Vulkan.IMAGE_VIEW_TYPE_2D
                      colFmt
                      Vulkan.IMAGE_ASPECT_COLOR_BIT
              let attachment = (swapchainImage, colorImageView)
              ( framebufferKey, framebuffer ) <- createFramebuffer device imGuiRenderPass swapchainExtent [colorImageView]
              pure ( [ imageViewKey, framebufferKey ], ( framebuffer, attachment ) )

      -------------------------------------------
      -- Create descriptor sets.

      -- Application doesn't have any descriptor sets of its own yet.

      -------------------------------------------
      -- Create pipelines.

      -- Application doesn't have any pipelines of its own yet.

      -------------------------------------------
      -- Return the resources and free method.

      pure
        ( do
            traverse_ ( traverse_ ResourceT.release ) fbKeys
            traverse_ ResourceT.release
              [ swapchainKey ]
        , SwapchainResources {..}
        )

  ( freeResources, resources@( SwapchainResources {..} ) ) <- swapchainResources Nothing
  let
    imageCount :: Word32
    imageCount = fromIntegral $ length swapchainImages

  logDebug "Allocating command buffers"
  commandBuffers <- snd <$> allocatePrimaryCommandBuffers device commandPool imageCount

  -------------------------------------------
  -- Initialise Dear ImGui.

  let
    initInfo :: ImGui.Vulkan.InitInfo
    initInfo = ImGui.Vulkan.InitInfo
      { instance'
      , physicalDevice
      , device
      , queueFamily
      , queue
      , pipelineCache  = Vulkan.NULL_HANDLE
      , descriptorPool = imGuiDescriptorPool
      , subpass        = 0
      , minImageCount
      , imageCount
      , msaaSamples    = Vulkan.SAMPLE_COUNT_1_BIT
      , mbAllocator    = Nothing
      , checkResult    = \case { Vulkan.SUCCESS -> pure (); e -> throw $ Vulkan.VulkanException e }
      }

  logDebug "Initialising ImGui SDL2 for Vulkan"
  void $ ResourceT.allocate
    ( ImGui.SDL.Vulkan.sdl2InitForVulkan window )
    ( const ImGui.SDL.sdl2Shutdown )

  logDebug "Initialising ImGui for Vulkan"
  ImGui.Vulkan.withVulkan initInfo imGuiRenderPass \ _ -> do

    logDebug "Running one-shot commands to upload ImGui textures"
    logDebug "Creating fence"
    ( fenceKey, fence ) <- createFence device
    logDebug "Allocating one-shot command buffer"
    ( fontUploadCommandBufferKey, fontUploadCommandBuffer ) <-
      second Boxed.Vector.head <$>
        allocatePrimaryCommandBuffers device commandPool 1

    logDebug "Recording one-shot commands"
    beginCommandBuffer fontUploadCommandBuffer
    _ <- ImGui.Vulkan.vulkanCreateFontsTexture fontUploadCommandBuffer
    endCommandBuffer fontUploadCommandBuffer

    logDebug "Submitting one-shot commands"
    submitCommandBuffer queue fontUploadCommandBuffer [] [] ( Just fence )
    waitForFences device ( WaitAll [ fence ] )

    logDebug "Finished uploading font objects"
    logDebug "Cleaning up one-shot commands"
    ImGui.Vulkan.vulkanDestroyFontUploadObjects
    traverse_ ResourceT.release [ fenceKey, fontUploadCommandBufferKey ]

    let
      mainLoop :: AppState m -> m ()
      mainLoop ( AppState {..} ) = do

        ( freeResources, resources@( SwapchainResources {..} ), freeOldResources ) <-
          if reloadSwapchain
          then do
            logDebug "Reloading swapchain and associated resources"
            ( freeNewResources, newResources ) <- swapchainResources ( Just resources )
            pure ( freeNewResources, newResources, freeOldResources *> freeResources )
          else pure ( freeResources, resources, freeOldResources )

        inputEvents <- map SDL.eventPayload <$> pollEventsWithImGui
        inputState  <- pure $ onSDLInputs inputState inputEvents

        unless ( quitAction inputState ) do
          ( acquireResult, nextImageIndex ) <-
            handleJust vulkanException ( \ e -> pure ( e, 0 ) )
              ( Vulkan.acquireNextImageKHR device swapchain maxBound nextImageSem Vulkan.NULL_HANDLE )
          let
            reloadSwapchain, quit :: Bool
            ( reloadSwapchain, quit ) = reloadQuit acquireResult
          unless quit do
            ( reloadSwapchain, quit ) <-
              if reloadSwapchain
              then do
                pure ( True, False )
              else
                handleJust vulkanException ( pure . reloadQuit ) do
                  let
                    commandBuffer :: Vulkan.CommandBuffer
                    commandBuffer = commandBuffers Boxed.Vector.! fromIntegral nextImageIndex
                    framebuffer :: Vulkan.Framebuffer
                    framebuffer = fst $ framebuffersWithAttachments Boxed.Vector.! fromIntegral nextImageIndex
                  Vulkan.resetCommandBuffer commandBuffer Vulkan.zero
                  beginCommandBuffer commandBuffer
                  cmdBeginRenderPass commandBuffer imGuiRenderPass framebuffer clearValues swapchainExtent

                  drawData <- gui
                  ImGui.Vulkan.vulkanRenderDrawData drawData commandBuffer Nothing

                  cmdEndRenderPass commandBuffer
                  endCommandBuffer commandBuffer
                  submitCommandBuffer
                    queue
                    commandBuffer
                    [ ( nextImageSem, Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ) ]
                    [ submitted ]
                    Nothing
                  presentResult <- present queue swapchain nextImageIndex [submitted]
                  Vulkan.queueWaitIdle queue
                  pure ( reloadQuit presentResult )
            freeOldResources
            let
              freeOldResources :: m ()
              freeOldResources = pure ()
            unless quit $ mainLoop ( AppState {..} )

    let
      reloadSwapchain :: Bool
      reloadSwapchain = False
      freeOldResources :: m ()
      freeOldResources = pure ()
      inputState :: Input
      inputState = nullInput

    logDebug "Starting main loop."
    mainLoop ( AppState {..} )


data SwapchainResources = SwapchainResources
  { swapchain       :: !Vulkan.SwapchainKHR
  , swapchainExtent :: !Vulkan.Extent2D
  , swapchainImages :: !( Boxed.Vector Vulkan.Image )
  , surfaceFormat   :: !Vulkan.SurfaceFormatKHR
  , imGuiRenderPass :: !Vulkan.RenderPass
  , framebuffersWithAttachments :: !( Boxed.Vector ( Vulkan.Framebuffer, ( Vulkan.Image, Vulkan.ImageView ) ) )
  }

data AppState m
  = AppState
    { reloadSwapchain  :: !Bool
    , freeResources    :: !( m () )
    , resources        :: !SwapchainResources
    , freeOldResources :: !( m () )
    , inputState       :: !Input
    }

pollEventsWithImGui :: MonadIO m => m [ SDL.Event ]
pollEventsWithImGui = do
  e <- ImGui.SDL.pollEventWithImGui
  case e of
    Nothing -> pure []
    Just e' -> ( e' : ) <$> pollEventsWithImGui

vulkanException :: Vulkan.VulkanException -> Maybe Vulkan.Result
vulkanException ( Vulkan.VulkanException e )
  | e >= Vulkan.SUCCESS
  = Nothing
  | otherwise
  = Just e

reloadQuit :: Vulkan.Result -> ( Bool, Bool )
reloadQuit = \ case
  Vulkan.ERROR_OUT_OF_DATE_KHR -> ( True , False )
  Vulkan.SUBOPTIMAL_KHR        -> ( True , False )
  e | e >= Vulkan.SUCCESS      -> ( False, False )
  _                            -> ( False, True  )
