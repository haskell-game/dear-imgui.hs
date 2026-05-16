{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Render (
    render,
) where

import Control.Exception (throwIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector as Vector
import qualified Vulkan
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import Vulkan.Utils.Pipeline (createColorPipelineFromShaders)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import qualified Vulkan.Utils.RenderPass as RenderPass
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)

import qualified DearImGui as ImGui
import qualified DearImGui.SDL as ImGui.SDL
import qualified DearImGui.Vulkan as ImGui.Vulkan

-- | Drive a recycling-Frame render loop drawing the colored triangle.
render ::
    VulkanContext ->
    -- | Initial swapchain
    Swapchain ->
    -- | Get current drawable size (for resize)
    IO Vk.Extent2D ->
    -- | Per-frame poller; 'True' means quit
    IO Bool ->
    ResourceT IO ()
render vc initialSC getDrawableSize shouldQuit = do
    (_, renderPass) <- RenderPass.createColorRenderPass vcDevice (KHR.format (sFormat initialSC)) Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
    (_, pipeline) <- createGraphicsPipeline vcDevice renderPass
    imGuiDescriptorPool <- createDescriptorPool vcDevice

    let
        imageCount = fromIntegral $ Vector.length (sImages initialSC)
        (QueueFamilyIndex gf, gq) = qGraphics vcQueues
        initInfo :: ImGui.Vulkan.InitInfo
        initInfo =
            ImGui.Vulkan.InitInfo
                { instance' = vcInstance
                , physicalDevice = vcPhysicalDevice
                , device = vcDevice
                , queueFamily = gf
                , queue = gq
                , pipelineCache = Vulkan.NULL_HANDLE
                , descriptorPool = imGuiDescriptorPool
                , subpass = 0
                , minImageCount = imageCount
                , imageCount
                , msaaSamples = Vulkan.SAMPLE_COUNT_1_BIT
                , mbAllocator = Nothing
                , rendering = Left renderPass
                , checkResult = \case
                    Vulkan.SUCCESS -> pure ()
                    e -> throwIO $ VulkanException e
                }
    _ <- allocate (ImGui.Vulkan.vulkanInit initInfo) ImGui.Vulkan.vulkanShutdown

    runWindowLoop
        vc
        initialSC
        getDrawableSize
        shouldQuit
        WindowLoop
            { wlMkState = \sc ->
                Framebuffer.createFramebuffers vcDevice renderPass (sImageViews sc) (sExtent sc)
            , wlRender = drawTriangle vc renderPass pipeline
            , wlOnFrame = noOnFrame
            , wlOnExit = noOnExit
            }
  where
    VulkanContext{..} = vc

----------------------------------------------------------------
-- Per-frame draw
----------------------------------------------------------------

drawTriangle ::
    VulkanContext ->
    Vk.RenderPass ->
    Vk.Pipeline ->
    Vector Vk.Framebuffer ->
    Frame ->
    ResourceT IO ()
drawTriangle vc renderPass pipeline framebuffers f = do
    let sc = fSwapchain f

    (acquireResult, imageIndex) <- acquireFrameImage vc f

    ImGui.Vulkan.vulkanNewFrame
    ImGui.SDL.sdl2NewFrame
    ImGui.newFrame

    -- Run your windows
    ImGui.showDemoWindow
    -- Process ImGui state into draw commands
    ImGui.render
    drawData <- ImGui.getDrawData

    commands <- recordCommands vc f \cb -> do
        let renderPassBeginInfo =
                zero
                    { Vk.renderPass = renderPass
                    , Vk.framebuffer = framebuffers V.! fromIntegral imageIndex
                    , Vk.renderArea = Vk.Rect2D{Vk.offset = zero, Vk.extent = sExtent sc}
                    , Vk.clearValues = [Vk.Color (Vk.Float32 0.1 0.1 0.1 0)]
                    }
        Vk.cmdUseRenderPass cb renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
            let
                Vk.Extent2D w h = sExtent sc
                vp = Vk.Viewport{Vk.x = 0, Vk.y = 0, Vk.width = realToFrac w, Vk.height = realToFrac h, Vk.minDepth = 0, Vk.maxDepth = 1}
                rect = Vk.Rect2D{Vk.offset = Vk.Offset2D 0 0, Vk.extent = sExtent sc}
            Vk.cmdSetViewport cb 0 [vp]
            Vk.cmdSetScissor cb 0 [rect]

            Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
            Vk.cmdDraw cb 3 1 0 0
            ImGui.Vulkan.vulkanRenderDrawData drawData cb Nothing

    queueSubmitFrame vc f [commands]
    presentFrameImage vc f acquireResult imageIndex

createGraphicsPipeline ::
    Vk.Device ->
    Vk.RenderPass ->
    ResourceT IO (ReleaseKey, Vk.Pipeline)
createGraphicsPipeline dev renderPass =
    createColorPipelineFromShaders
        dev
        renderPass
        [ (Vk.SHADER_STAGE_VERTEX_BIT, vertCode)
        , (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)
        ]
  where
    vertCode =
        [vert|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) out vec3 fragColor;

        vec2 positions[3] = vec2[](
          vec2(0.0, -0.5),
          vec2(0.5, 0.5),
          vec2(-0.5, 0.5)
        );
        vec3 colors[3] = vec3[](
          vec3(1.0, 1.0, 0.0),
          vec3(0.0, 1.0, 1.0),
          vec3(1.0, 0.0, 1.0)
        );

        void main() {
          gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
          fragColor   = colors[gl_VertexIndex];
        }
      |]
    fragCode =
        [frag|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) in vec3 fragColor;
        layout(location = 0) out vec4 outColor;

        void main() {
            outColor = vec4(fragColor, 1.0);
        }
      |]

createDescriptorPool :: Vulkan.Device -> ResourceT IO Vulkan.DescriptorPool
createDescriptorPool device = snd <$> Vulkan.withDescriptorPool device createInfo Nothing allocate
  where
    createInfo :: Vulkan.DescriptorPoolCreateInfo '[]
    createInfo =
        Vulkan.DescriptorPoolCreateInfo
            { Vulkan.next = ()
            , Vulkan.flags = Vulkan.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
            , Vulkan.poolSizes = Vector.fromList poolSizes
            , Vulkan.maxSets
            }
    maxSets = 2
    poolSizes = do
        descType <- types
        pure
            Vulkan.DescriptorPoolSize
                { Vulkan.type' = descType
                , Vulkan.descriptorCount = fromIntegral $ maxSets * 1000
                }
    types =
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
