{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.GLFW

GLFW specific functions backend for Dear ImGui.

Modules for initialising a backend with GLFW can be found under the corresponding backend,
e.g. "DearImGui.GLFW.OpenGL".
-}

module DearImGui.GLFW (
    -- ** GLFW
    glfwNewFrame
  , glfwShutdown
  )
  where

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )


C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_glfw.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplGlfw_NewFrame@.
glfwNewFrame :: MonadIO m => m ()
glfwNewFrame = liftIO do
  [C.exp| void { ImGui_ImplGlfw_NewFrame(); } |]


-- | Wraps @ImGui_ImplGlfw_Shutdown@.
glfwShutdown :: MonadIO m => m ()
glfwShutdown = liftIO do
  [C.exp| void { ImGui_ImplGlfw_Shutdown(); } |]