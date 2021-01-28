{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.OpenGL

OpenGL 3 backend for Dear ImGui.
-}

module DearImGui.OpenGL3
  ( openGL3Init
  , openGL3Shutdown
  , openGL3NewFrame
  , openGL3RenderDrawData
  )
  where

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- DearImGui
import DearImGui
  ( DrawData(..) )


C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "GL/glew.h"
C.include "backends/imgui_impl_opengl3.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplOpenGL3_Init@.
openGL3Init :: MonadIO m => m Bool
openGL3Init = liftIO $
  ( 0 /= ) <$> [C.block| bool {
    glewInit();
    return ImGui_ImplOpenGL3_Init();
  } |]


-- | Wraps @ImGui_ImplOpenGL3_Shutdown@.
openGL3Shutdown :: MonadIO m => m ()
openGL3Shutdown = liftIO do
  [C.exp| void { ImGui_ImplOpenGL3_Shutdown(); } |]


-- | Wraps @ImGui_ImplOpenGL3_NewFrame@.
openGL3NewFrame :: MonadIO m => m ()
openGL3NewFrame = liftIO do
  [C.exp| void { ImGui_ImplOpenGL3_NewFrame(); } |]


-- | Wraps @ImGui_ImplOpenGL3_RenderDrawData@.
openGL3RenderDrawData :: MonadIO m => DrawData -> m ()
openGL3RenderDrawData (DrawData ptr) = liftIO do
  [C.exp| void { ImGui_ImplOpenGL3_RenderDrawData((ImDrawData*) $( void* ptr )) } |]
