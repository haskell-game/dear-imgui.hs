{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGUI.SDL.OpenGL

Initialising the OpenGL backend for Dear ImGui using SDL2.
-}

module DearImGui.SDL.OpenGL
  ( sdl2InitForOpenGL )
  where

-- base
import Foreign.Ptr
  ( Ptr )
import Unsafe.Coerce
  ( unsafeCoerce )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- sdl2
import SDL
  ( GLContext )
import SDL.Internal.Types
  ( Window(..) )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )


C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_opengl2.h"
C.include "backends/imgui_impl_sdl2.h"
C.include "SDL.h"
C.include "SDL_opengl.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplSDL2_InitForOpenGL@.
sdl2InitForOpenGL :: MonadIO m => Window -> GLContext -> m Bool
sdl2InitForOpenGL (Window windowPtr) glContext = liftIO do
  ( 0 /= ) <$> [C.exp| bool { ImGui_ImplSDL2_InitForOpenGL((SDL_Window*)$(void* windowPtr), $(void* glContextPtr)) } |]
  where
    glContextPtr :: Ptr ()
    glContextPtr = unsafeCoerce glContext
