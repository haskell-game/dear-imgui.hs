{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
  ( ReleaseKey, ResourceT, allocate, allocate_, register, release, runResourceT )
import Data.Word (Word32)
import Data.Bits ((.|.))
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Foreign (castPtr, copyBytes, withForeignPtr)
import qualified Graphics.UI.GLFW as GLFW

import qualified Codec.Picture as Picture

import Vulkan (pattern API_VERSION_1_3)
import qualified Vulkan as Vk
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Zero (zero)
import Vulkan.Utils.Barrier (imageBarrier)
import Vulkan.Utils.Frame
import Vulkan.Utils.Framebuffer (allocateFramebuffer)
import qualified Vulkan.Utils.Init.GLFW as Init
import Vulkan.Utils.Init.GLFW.Window (createWindow, drawableSize, showWindow, withGLFW)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..), allocateDevice)
import Vulkan.Utils.RenderPass (allocateColorRenderPass)
import Vulkan.Utils.Swapchain
import Vulkan.Utils.VulkanContext (VulkanContext (..), mkVulkanContext)
import Vulkan.Utils.WindowLoop
import qualified VulkanMemoryAllocator as VMA
import VulkanMemoryAllocator.Utils (allocatorCreateInfo)

import DearImGui.Impl.Init (InitFailed (..))
import qualified DearImGui as ImGui
import qualified DearImGui.Impl.GLFW as ImplGlfw
import qualified DearImGui.Impl.Vulkan as ImplVulkan

main :: IO ()
main = runResourceT do
  withGLFW
  window <- createWindow "DearImGui - Vulkan" 1280 720

  inst <- Init.allocateInstance window (Just appInfo) frameInstanceRequirements []
  surface <- Init.allocateSurface inst window
  (phys, dev, queues) <- allocateDevice inst (Just surface) frameDeviceRequirements
  vc <- liftIO $ mkVulkanContext inst phys dev queues

  windowSize <- drawableSize window
  let swapchainConfig = defaultSwapchainConfig { scSurfaceFormatPreferences = [ImplVulkan.surfaceFormatPreference] }
  swapchain <- allocateSwapchain phys dev swapchainConfig Vk.NULL_HANDLE windowSize surface
  (_, renderPass) <- allocateColorRenderPass dev (SurfaceFormatKHR.format (sFormat swapchain)) Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR

  (_, vma) <- VMA.withAllocator (allocatorCreateInfo zero apiVersion inst phys dev) allocate
  (textureSize, textureView) <- uploadTexture vc vma (juliaSet 512 384)

  descriptorPool <- ImplVulkan.mkDescriptorPool dev 1
  void $ allocate ImGui.createContext ImGui.destroyContext
  void $ allocate_
    ( ImplGlfw.initForVulkan window True >>= \initialized ->
        unless initialized $ throwIO (InitFailed "ImGui_ImplGlfw_InitForVulkan")
    )
    ImplGlfw.shutdown
  let (QueueFamilyIndex graphicsFamily, graphicsQueue) = qGraphics queues
  void $ allocate_
    ( ImplVulkan.init descriptorPool ImplVulkan.InitArgs
        { apiVersion
        , inst
        , physicalDevice = phys
        , device = dev
        , queueFamily = graphicsFamily
        , queue = graphicsQueue
        , imageCount = swapchainImageCount swapchain
        , target = ImplVulkan.RenderPassTarget renderPass
        }
    )
    ImplVulkan.shutdown
  Vk.DescriptorSet textureSet <- liftIO $ ImplVulkan.addTexture textureView Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  showWindow window
  runWindowLoop vc swapchain (drawableSize window) (windowClosed window) WindowLoop
    { wlMkState = allocateFramebuffers dev renderPass
    , wlMkRecycled = noRecycledResources
    , wlRender = renderFrame vc renderPass textureSize (fromIntegral textureSet)
    , wlOnFrame = noOnFrame
    , wlOnExit = noOnExit
    }
  where
    apiVersion = API_VERSION_1_3
    appInfo = zero { Vk.applicationName = Just "DearImGui - Vulkan", Vk.apiVersion = apiVersion }

swapchainImageCount :: Swapchain -> Word32
swapchainImageCount = fromIntegral . length . sImages

windowClosed :: GLFW.Window -> IO Bool
windowClosed window = GLFW.pollEvents *> GLFW.windowShouldClose window

allocateFramebuffers :: Vk.Device -> Vk.RenderPass -> Swapchain -> ResourceT IO (V.Vector Vk.Framebuffer, ReleaseKey)
allocateFramebuffers dev renderPass swapchain@Swapchain{sImageViews, sExtent} = do
  liftIO $ ImplVulkan.setMinImageCount (swapchainImageCount swapchain)
  (keys, framebuffers) <- V.unzip <$> traverse (\view -> allocateFramebuffer dev renderPass view sExtent) sImageViews
  key <- register (traverse_ release keys)
  pure (framebuffers, key)

renderFrame :: VulkanContext () -> Vk.RenderPass -> ImGui.ImVec2 -> ImGui.ImTextureID -> V.Vector Vk.Framebuffer -> Frame () -> ResourceT IO ()
renderFrame vc renderPass textureSize textureId framebuffers frame = do
  (acquireResult, imageIndex) <- acquireFrameImage vc frame
  let
    renderPassBegin = zero
      { Vk.renderPass = renderPass
      , Vk.framebuffer = framebuffers V.! fromIntegral imageIndex
      , Vk.renderArea = Vk.Rect2D zero (sExtent (fSwapchain frame))
      , Vk.clearValues = [Vk.Color (Vk.Float32 0.5 0.2 0 1)]
      }
  commandBuffer <- recordCommands vc frame \cb ->
    Vk.cmdUseRenderPass cb renderPassBegin Vk.SUBPASS_CONTENTS_INLINE do
      drawData <- gui textureSize textureId
      liftIO $ ImplVulkan.renderDrawData drawData (Vk.commandBufferHandle cb) Vk.NULL_HANDLE
  queueSubmitFrame vc frame imageIndex [commandBuffer]
  presentFrameImage vc frame acquireResult imageIndex

gui :: ImGui.ImVec2 -> ImGui.ImTextureID -> ResourceT IO ImGui.DrawData
gui textureSize textureId = do
  liftIO ImplVulkan.newFrame
  liftIO ImplGlfw.newFrame
  ImGui.newFrame

  ImGui.showDemoWindow
  ImGui.withWindowOpen "Vulkan demo" do
    clicked <-
      ImGui.imageButton
        "##btn"
        (ImGui.textureRefFromID textureId)
        textureSize
        (ImGui.ImVec2 0 0)
        (ImGui.ImVec2 1 1)
        (ImGui.ImVec4 0 0 0 0)
        (ImGui.ImVec4 1 1 1 1)
    when clicked $
      ImGui.text "clicky click!"

  ImGui.render
  ImGui.getDrawData

juliaSet :: Int -> Int -> Picture.Image Picture.PixelRGBA8
juliaSet width height = Picture.generateImage pixel width height
  where
    pixel px py = maybe transparent cosinePalette (escapeTime 0 (plane px width 1.6 :+ plane py height 1.2))
    plane i n half = (fromIntegral i / fromIntegral n * 2 - 1) * half
    transparent = Picture.PixelRGBA8 0 0 0 0
    c = (-0.7269) :+ 0.1889
    maxIter = 128 :: Int
    escapeTime :: Int -> Complex Double -> Maybe Double
    escapeTime n z
      | n >= maxIter = Nothing
      | r2 > 16 = Just ((fromIntegral n + 1 - logBase 2 (logBase 2 (sqrt r2))) / fromIntegral maxIter)
      | otherwise = escapeTime (n + 1) (z * z + c)
      where
        r2 = realPart z * realPart z + imagPart z * imagPart z

cosinePalette :: Double -> Picture.PixelRGBA8
cosinePalette t = Picture.PixelRGBA8 (channel 0) (channel 0.1) (channel 0.2) 255
  where
    channel phase = round (255 * (0.5 + 0.5 * cos (2 * pi * (1.2 * sqrt t + phase))))

uploadTexture :: VulkanContext rr -> VMA.Allocator -> Picture.Image Picture.PixelRGBA8 -> ResourceT IO (ImGui.ImVec2, Vk.ImageView)
uploadTexture vc vma picture = do
  let
    width = fromIntegral (Picture.imageWidth picture)
    height = fromIntegral (Picture.imageHeight picture)
    format = Vk.FORMAT_R8G8B8A8_SRGB
    extent = Vk.Extent3D width height 1
    (pixels, pixelBytes) = SV.unsafeToForeignPtr0 (Picture.imageData picture)
    dev = vcDevice vc
    copyRegion = Vk.BufferImageCopy 0 0 0 (Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1) zero extent

  (_, (image, _, _)) <- VMA.withImage vma
    zero
      { Vk.imageType = Vk.IMAGE_TYPE_2D
      , Vk.mipLevels = 1
      , Vk.arrayLayers = 1
      , Vk.format = format
      , Vk.extent = extent
      , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
      , Vk.usage = Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT
      , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
      }
    zero { VMA.usage = VMA.MEMORY_USAGE_GPU_ONLY, VMA.requiredFlags = Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT }
    allocate

  liftIO $ runResourceT do
    (_, (staging, stagingAllocation, stagingInfo)) <- VMA.withBuffer vma
      zero { Vk.size = fromIntegral pixelBytes, Vk.usage = Vk.BUFFER_USAGE_TRANSFER_SRC_BIT }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        , VMA.requiredFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        }
      allocate
    liftIO $ withForeignPtr pixels \src -> copyBytes (VMA.mappedData stagingInfo) (castPtr src) pixelBytes
    VMA.flushAllocation vma stagingAllocation 0 Vk.WHOLE_SIZE

    oneShot vc \cb -> do
      Vk.cmdPipelineBarrier cb Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT Vk.PIPELINE_STAGE_TRANSFER_BIT zero [] []
        [ imageBarrier Vk.IMAGE_ASPECT_COLOR_BIT zero Vk.ACCESS_TRANSFER_WRITE_BIT
            Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL image
        ]
      Vk.cmdCopyBufferToImage cb staging image Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (V.singleton copyRegion)
      Vk.cmdPipelineBarrier cb Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT zero [] []
        [ imageBarrier Vk.IMAGE_ASPECT_COLOR_BIT Vk.ACCESS_TRANSFER_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT
            Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL image
        ]

  (_, view) <- Vk.withImageView dev
    zero
      { Vk.image = image
      , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
      , Vk.format = format
      , Vk.subresourceRange = zero { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT, Vk.levelCount = 1, Vk.layerCount = 1 }
      }
    Nothing
    allocate
  pure (ImGui.ImVec2 (fromIntegral width) (fromIntegral height), view)

oneShot :: VulkanContext rr -> (Vk.CommandBuffer -> IO ()) -> ResourceT IO ()
oneShot VulkanContext{vcDevice, vcQueues} record = do
  let (QueueFamilyIndex family, queue) = qGraphics vcQueues
  pool <- allocateCommandPool vcDevice family
  cb <- allocatePrimary vcDevice pool
  liftIO (record cb)
  Vk.endCommandBuffer cb
  (_, fence) <- Vk.withFence vcDevice zero Nothing allocate
  Vk.queueSubmit queue [SomeStruct zero { Vk.commandBuffers = [Vk.commandBufferHandle cb] }] fence
  void $ Vk.waitForFences vcDevice [fence] True maxBound
