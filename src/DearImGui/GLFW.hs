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

    -- $callbacks
  , glfwWindowFocusCallback
  , glfwCursorEnterCallback
  , glfwCursorPosCallback
  , glfwMouseButtonCallback
  , glfwScrollCallback
  , glfwKeyCallback
  , glfwCharCallback
  , glfwMonitorCallback
  )
  where

-- base
import Foreign
  ( Ptr, castPtr )
import Foreign.C.Types
  ( CInt, CDouble, CUInt )
import Unsafe.Coerce (unsafeCoerce)

-- bindings-GLFW
import Bindings.GLFW
  ( C'GLFWmonitor, C'GLFWwindow )

-- GLFW-b
import Graphics.UI.GLFW
  ( Monitor, Window )

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

-- $callbacks
-- == GLFW callbacks
-- * When calling Init with @install_callbacks=true@:
--   GLFW callbacks will be installed for you.
--   They will call user's previously installed callbacks, if any.
-- * When calling Init with @install_callbacks=false@:
--   GLFW callbacks won't be installed.
--   You will need to call those function yourself from your own GLFW callbacks.

-- | Wraps @ImGui_ImplGlfw_Shutdown@.
glfwShutdown :: MonadIO m => m ()
glfwShutdown = liftIO do
  [C.exp| void { ImGui_ImplGlfw_Shutdown(); } |]

glfwWindowFocusCallback :: MonadIO m => Window -> CInt -> m ()
glfwWindowFocusCallback window focused = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_WindowFocusCallback(
      static_cast<GLFWwindow *>(
        $(void * windowPtr)
      ),
      $(int focused)
    );
  } |]
  where
    windowPtr = castPtr $ unWindow window

glfwCursorEnterCallback :: MonadIO m => Window -> CInt -> m ()
glfwCursorEnterCallback window entered = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_CursorEnterCallback(
      static_cast<GLFWwindow *>(
        $(void * windowPtr)
      ),
      $(int entered)
    );
  } |]
  where
    windowPtr = castPtr $ unWindow window

glfwCursorPosCallback :: MonadIO m => Window -> CDouble -> CDouble -> m ()
glfwCursorPosCallback window x y = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_CursorPosCallback(
      static_cast<GLFWwindow *>(
        $(void * windowPtr)
      ),
      $(double x),
      $(double y)
    );
  } |]
  where
    windowPtr = castPtr $ unWindow window

glfwMouseButtonCallback :: MonadIO m => Window -> CInt -> CInt -> CInt -> m ()
glfwMouseButtonCallback window button action mods = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_MouseButtonCallback(
      static_cast<GLFWwindow *>(
        $(void * windowPtr)
      ),
      $(int button),
      $(int action),
      $(int mods)
    );
  } |]
  where
    windowPtr = castPtr $ unWindow window

glfwScrollCallback :: MonadIO m => Window -> CDouble -> CDouble -> m ()
glfwScrollCallback window xoffset yoffset = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_ScrollCallback(
      static_cast<GLFWwindow *>(
        $(void * windowPtr)
      ),
      $(double xoffset),
      $(double yoffset)
    );
  } |]
  where
    windowPtr = castPtr $ unWindow window

glfwKeyCallback :: MonadIO m => Window -> CInt -> CInt -> CInt -> CInt -> m ()
glfwKeyCallback window key scancode action mods = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_KeyCallback(
      static_cast<GLFWwindow *>(
        $(void * windowPtr)
      ),
      $(int key),
      $(int scancode),
      $(int action),
      $(int mods)
    );
  } |]
  where
    windowPtr = castPtr $ unWindow window

glfwCharCallback :: MonadIO m => Window -> CUInt -> m ()
glfwCharCallback window c = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_CharCallback(
      static_cast<GLFWwindow *>(
        $(void * windowPtr)
      ),
      $(unsigned int c)
    );
  } |]
  where
    windowPtr = castPtr $ unWindow window

glfwMonitorCallback :: MonadIO m => Monitor -> CInt -> m ()
glfwMonitorCallback monitor event = liftIO do
  [C.exp| void {
    ImGui_ImplGlfw_MonitorCallback(
      static_cast<GLFWmonitor *>(
        $(void * monitorPtr)
      ),
      $(int event)
    );
  } |]
  where
    monitorPtr = castPtr $ unMonitor monitor

-- | Strip the unpublished newtype wrapper.
unWindow :: Window -> Ptr C'GLFWwindow
unWindow = unsafeCoerce

-- | Strip the unpublished newtype wrapper.
unMonitor :: Monitor -> Ptr C'GLFWmonitor
unMonitor = unsafeCoerce
