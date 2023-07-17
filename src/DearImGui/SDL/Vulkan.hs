{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.SDL.Vulkan

Initialising the Vulkan backend for Dear ImGui using SDL2.
-}

module DearImGui.SDL.Vulkan
  ( sdl2InitForVulkan )
  where

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- sdl2
import SDL.Internal.Types
  ( Window(..) )

-- transformers
import Control.Monad.IO.Class ( MonadIO, liftIO )


C.context Cpp.cppCtx
C.include "imgui.h"
C.include "backends/imgui_impl_vulkan.h"
C.include "backends/imgui_impl_sdl2.h"
C.include "SDL.h"
C.include "SDL_vulkan.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplSDL2_InitForVulkan@.
sdl2InitForVulkan :: MonadIO m => Window -> m Bool
sdl2InitForVulkan (Window windowPtr) = liftIO do
  ( 0 /= ) <$> [C.exp| bool { ImGui_ImplSDL2_InitForVulkan((SDL_Window*)$(void* windowPtr)) } |]
