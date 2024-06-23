{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.Vulkan

Vulkan backend for Dear ImGui.
-}

module DearImGui.Vulkan
  ( InitInfo(..)
  , withVulkan
  , vulkanInit
  , vulkanShutdown
  , vulkanNewFrame
  , vulkanRenderDrawData
  , vulkanCreateFontsTexture
  , vulkanDestroyFontUploadObjects
  , vulkanSetMinImageCount

  , vulkanAddTexture
  )
  where

-- base
import Data.Maybe
  ( fromMaybe )
import Data.Word
  ( Word32 )
import Foreign.Marshal.Alloc
  ( alloca )
import Foreign.Marshal.Utils
  ( fromBool )
import Foreign.Ptr
  ( FunPtr, Ptr, freeHaskellFunPtr, nullPtr )
import Foreign.Storable
  ( poke )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- unliftio
import UnliftIO
  ( MonadUnliftIO )
import UnliftIO.Exception
  ( bracket )

-- vulkan
import qualified Vulkan

-- DearImGui
import DearImGui
  ( DrawData(..) )
import DearImGui.Vulkan.Types
  ( vulkanCtx )


C.context ( Cpp.cppCtx <> C.funCtx <> vulkanCtx )
C.include "imgui.h"
C.include "backends/imgui_impl_vulkan.h"
Cpp.using "namespace ImGui"


data InitInfo =
  InitInfo
  { instance'             :: !Vulkan.Instance
  , physicalDevice        :: !Vulkan.PhysicalDevice
  , device                :: !Vulkan.Device
  , queueFamily           :: !Word32
  , queue                 :: !Vulkan.Queue
  , pipelineCache         :: !Vulkan.PipelineCache
  , descriptorPool        :: !Vulkan.DescriptorPool
  , subpass               :: !Word32
  , minImageCount         :: !Word32
  , imageCount            :: !Word32
  , msaaSamples           :: !Vulkan.SampleCountFlagBits
  , colorAttachmentFormat :: !(Maybe Vulkan.Format)
  , useDynamicRendering   :: !Bool
  , mbAllocator           :: Maybe Vulkan.AllocationCallbacks
  , checkResult           :: Vulkan.Result -> IO ()
  }

-- | Wraps @ImGui_ImplVulkan_Init@ and @ImGui_ImplVulkan_Shutdown@.
withVulkan :: MonadUnliftIO m => InitInfo -> Vulkan.RenderPass -> ( Bool -> m a ) -> m a
withVulkan initInfo renderPass action =
  bracket
    ( vulkanInit initInfo renderPass )
    vulkanShutdown
    ( \ ( _, initResult ) -> action initResult )

-- | Wraps @ImGui_ImplVulkan_Init@.
--
-- Use 'vulkanShutdown' to clean up on shutdown.
-- Prefer using 'withVulkan' when possible, as it automatically handles cleanup.
vulkanInit :: MonadIO m => InitInfo -> Vulkan.RenderPass -> m (FunPtr (Vulkan.Result -> IO ()), Bool)
vulkanInit ( InitInfo {..} ) renderPass = do
  let
    instancePtr :: Ptr Vulkan.Instance_T
    instancePtr = Vulkan.instanceHandle instance'
    physicalDevicePtr :: Ptr Vulkan.PhysicalDevice_T
    physicalDevicePtr = Vulkan.physicalDeviceHandle physicalDevice
    devicePtr :: Ptr Vulkan.Device_T
    devicePtr = Vulkan.deviceHandle device
    queuePtr :: Ptr Vulkan.Queue_T
    queuePtr = Vulkan.queueHandle queue
    withCallbacks :: ( Ptr Vulkan.AllocationCallbacks -> IO a ) -> IO a
    withCallbacks f = case mbAllocator of
      Nothing        -> f nullPtr
      Just callbacks -> alloca ( \ ptr -> poke ptr callbacks *> f ptr )
    useDynamicRendering' :: Cpp.CBool
    useDynamicRendering' = fromBool useDynamicRendering
    colorAttachmentFormat' :: Vulkan.Format
    colorAttachmentFormat' = fromMaybe Vulkan.FORMAT_UNDEFINED colorAttachmentFormat
  liftIO do
    checkResultFunPtr <- $( C.mkFunPtr [t| Vulkan.Result -> IO () |] ) checkResult
    initResult <- withCallbacks \ callbacksPtr ->
        [C.block| bool {
          ImGui_ImplVulkan_InitInfo initInfo;
          VkInstance instance = { $( VkInstance_T* instancePtr ) };
          initInfo.Instance = instance;
          VkPhysicalDevice physicalDevice = { $( VkPhysicalDevice_T* physicalDevicePtr ) };
          initInfo.PhysicalDevice = physicalDevice;
          VkDevice device = { $( VkDevice_T* devicePtr ) };
          initInfo.Device = device;
          initInfo.QueueFamily = $(uint32_t queueFamily);
          VkQueue queue = { $( VkQueue_T* queuePtr ) };
          initInfo.Queue = queue;
          initInfo.PipelineCache = $(VkPipelineCache pipelineCache);
          initInfo.DescriptorPool = $(VkDescriptorPool descriptorPool);
          initInfo.Subpass = $(uint32_t subpass);
          initInfo.MinImageCount = $(uint32_t minImageCount);
          initInfo.ImageCount = $(uint32_t imageCount);
          initInfo.MSAASamples = $(VkSampleCountFlagBits msaaSamples);
          initInfo.Allocator = $(VkAllocationCallbacks* callbacksPtr);
          initInfo.CheckVkResultFn = $( void (*checkResultFunPtr)(VkResult) );
          initInfo.UseDynamicRendering = $(bool useDynamicRendering');
          initInfo.ColorAttachmentFormat = $(VkFormat colorAttachmentFormat');
          return ImGui_ImplVulkan_Init(&initInfo, $(VkRenderPass renderPass) );
        }|]
    pure ( checkResultFunPtr, initResult /= 0 )

-- | Wraps @ImGui_ImplVulkan_Shutdown@.
--
-- Counterpart to 'vulkanInit', for clean-up.
vulkanShutdown :: MonadIO m => (FunPtr a, b) -> m ()
vulkanShutdown ( checkResultFunPtr, _ ) = liftIO do
  [C.exp| void { ImGui_ImplVulkan_Shutdown(); } |]
  freeHaskellFunPtr checkResultFunPtr

-- | Wraps @ImGui_ImplVulkan_NewFrame@.
vulkanNewFrame :: MonadIO m => m ()
vulkanNewFrame = liftIO do
  [C.exp| void { ImGui_ImplVulkan_NewFrame(); } |]

-- | Wraps @ImGui_ImplVulkan_RenderDrawData@.
vulkanRenderDrawData :: MonadIO m => DrawData -> Vulkan.CommandBuffer -> Maybe Vulkan.Pipeline -> m ()
vulkanRenderDrawData (DrawData dataPtr) commandBuffer mbPipeline = liftIO do
  let
    commandBufferPtr :: Ptr Vulkan.CommandBuffer_T
    commandBufferPtr = Vulkan.commandBufferHandle commandBuffer
    pipeline :: Vulkan.Pipeline
    pipeline = fromMaybe Vulkan.NULL_HANDLE mbPipeline
  [C.block| void {
    VkCommandBuffer commandBuffer = { $( VkCommandBuffer_T* commandBufferPtr ) };
    ImGui_ImplVulkan_RenderDrawData((ImDrawData*) $(void* dataPtr), commandBuffer, $(VkPipeline pipeline));
  }|]

-- | Wraps @ImGui_ImplVulkan_CreateFontsTexture@.
vulkanCreateFontsTexture :: MonadIO m => Vulkan.CommandBuffer -> m Bool
vulkanCreateFontsTexture commandBuffer = liftIO do
  let
    commandBufferPtr :: Ptr Vulkan.CommandBuffer_T
    commandBufferPtr = Vulkan.commandBufferHandle commandBuffer
  res <-
    [C.block| bool {
      VkCommandBuffer commandBuffer = { $( VkCommandBuffer_T* commandBufferPtr ) };
      return ImGui_ImplVulkan_CreateFontsTexture(commandBuffer);
    }|]
  pure ( res /= 0 )

-- | Wraps @ImGui_ImplVulkan_DestroyFontUploadObjects@.
vulkanDestroyFontUploadObjects :: MonadIO m => m ()
vulkanDestroyFontUploadObjects = liftIO do
  [C.exp| void { ImGui_ImplVulkan_DestroyFontUploadObjects(); } |]

-- | Wraps @ImGui_ImplVulkan_SetMinImageCount@.
vulkanSetMinImageCount :: MonadIO m => Word32 -> m ()
vulkanSetMinImageCount minImageCount = liftIO do
  [C.exp| void { ImGui_ImplVulkan_SetMinImageCount($(uint32_t minImageCount)); } |]

-- | Wraps @ImGui_ImplVulkan_AddTexture@.
vulkanAddTexture :: MonadIO m => Vulkan.Sampler -> Vulkan.ImageView -> Vulkan.ImageLayout -> m Vulkan.DescriptorSet
vulkanAddTexture sampler imageView imageLayout = liftIO do
  [C.block|
    VkDescriptorSet {
      return ImGui_ImplVulkan_AddTexture(
        $(VkSampler sampler),
        $(VkImageView imageView),
        $(VkImageLayout imageLayout)
      );
    }
  |]
