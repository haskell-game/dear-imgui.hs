{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGUI.GLFW.OpenGL

Initialising the OpenGL backend for Dear ImGui using GLFW3.
-}

module DearImGui.GLFW.OpenGL
  ( glfwInitForOpenGL )
  where

-- base
import Data.Bool
  ( bool )
import Foreign.C.Types
  ( CBool )
import Foreign.Ptr
  ( Ptr )
import Unsafe.Coerce
  ( unsafeCoerce )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- GLFW
import Graphics.UI.GLFW
  ( Window )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )


C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_opengl2.h"
C.include "backends/imgui_impl_glfw.h"
C.include "GLFW/glfw3.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplGlfw_InitForOpenGL@.
glfwInitForOpenGL :: MonadIO m => Window -> Bool -> m Bool
glfwInitForOpenGL window installCallbacks = liftIO do
  ( 0 /= ) <$> [C.exp| bool { ImGui_ImplGlfw_InitForOpenGL((GLFWwindow*)$(void* windowPtr), $(bool cInstallCallbacks)) } |]
  where
    windowPtr :: Ptr ()
    windowPtr = unsafeCoerce window

    cInstallCallbacks :: CBool
    cInstallCallbacks = bool 0 1 installCallbacks
