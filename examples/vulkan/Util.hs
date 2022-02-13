{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

-- base
import Data.Coerce
  ( coerce )
import Data.Functor.Compose
  ( Compose(..) )
import Data.Functor.Identity
  ( Identity(..) )
import Data.Traversable
  ( for )
#if MIN_VERSION_VulkanMemoryAllocator(0,8,0)
import Foreign
  ( castFunPtr )
#endif

-- transformers
import Control.Monad.Trans.State.Strict
  ( StateT(..), State, evalState )
import Control.Monad.Trans.Writer.Strict
  ( runWriter, tell )

-- vulkan
import qualified Vulkan
#if MIN_VERSION_VulkanMemoryAllocator(0,8,0)
import qualified Vulkan.Dynamic as VkDynamic
#endif
import Vulkan.Zero (zero)

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as VMA

---------------------------------------------------------------

iunzipWith
  :: (Traversable t, Num i, Enum i)
  => (i -> a -> b) -> (i -> a -> c) -> t a -> ( t b, [c] )
iunzipWith f g ta
  = runWriter
  $ ifor 0 succ ta \ i a -> do
       tell [g i a]
       pure ( f i a )

ifor
  :: forall t f i a b
  .  ( Applicative f, Traversable t )
  => i -> ( i -> i ) -> t a -> ( i -> a -> f b ) -> f (t b)
ifor i0 upd ta f = (`evalState` i0) . getCompose $ result
  where
    result :: Compose (State i) f (t b)
    result = for ta \ a -> ( coerce ( \ i -> ( f i a, upd i ) ) )

vmaVulkanFunctions
  :: Vulkan.Device
  -> Vulkan.Instance
  -> VMA.VulkanFunctions
#if MIN_VERSION_VulkanMemoryAllocator(0,8,0)
vmaVulkanFunctions Vulkan.Device{deviceCmds} Vulkan.Instance{instanceCmds} =
  zero
    { VMA.vkGetInstanceProcAddr =
        castFunPtr $ VkDynamic.pVkGetInstanceProcAddr instanceCmds
    , VMA.vkGetDeviceProcAddr =
        castFunPtr $ VkDynamic.pVkGetDeviceProcAddr deviceCmds
    }
#else
vmaVulkanFunctions _device _instance = zero
#endif
