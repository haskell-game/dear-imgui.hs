{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DearImGui.Vulkan.Types
  ( vulkanCtx )
  where

-- containers
import qualified Data.Map.Strict as Map
  ( fromList )

-- inline-c
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types          as C

-- vulkan
import qualified Vulkan

vulkanTypesTable :: C.TypesTable
vulkanTypesTable = Map.fromList
  [ ( C.TypeName "VkAllocationCallbacks", [t| Vulkan.AllocationCallbacks |] )
  , ( C.TypeName "VkCommandBuffer_T"    , [t| Vulkan.CommandBuffer_T     |] )
  , ( C.TypeName "VkDescriptorPool"     , [t| Vulkan.DescriptorPool      |] )
  , ( C.TypeName "VkDevice_T"           , [t| Vulkan.Device_T            |] )
  , ( C.TypeName "VkInstance_T"         , [t| Vulkan.Instance_T          |] )
  , ( C.TypeName "VkPhysicalDevice_T"   , [t| Vulkan.PhysicalDevice_T    |] )
  , ( C.TypeName "VkPipeline"           , [t| Vulkan.Pipeline            |] )
  , ( C.TypeName "VkPipelineCache"      , [t| Vulkan.PipelineCache       |] )
  , ( C.TypeName "VkQueue_T"            , [t| Vulkan.Queue_T             |] )
  , ( C.TypeName "VkRenderPass"         , [t| Vulkan.RenderPass          |] )
  , ( C.TypeName "VkResult"             , [t| Vulkan.Result              |] )
  , ( C.TypeName "VkSampleCountFlagBits", [t| Vulkan.SampleCountFlagBits |] )
  , ( C.TypeName "VkSampler"            , [t| Vulkan.Sampler             |] )
  , ( C.TypeName "VkImageView"          , [t| Vulkan.ImageView           |] )
  , ( C.TypeName "VkImageLayout"        , [t| Vulkan.ImageLayout         |] )
  , ( C.TypeName "VkDescriptorSet"      , [t| Vulkan.DescriptorSet       |] )
  ]

vulkanCtx :: C.Context
vulkanCtx = mempty { C.ctxTypesTable = vulkanTypesTable }
