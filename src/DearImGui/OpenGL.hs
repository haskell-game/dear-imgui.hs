{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.OpenGL

OpenGL backend for Dear ImGui.
-}

module DearImGui.OpenGL
  ( openGL2Init
  , openGL2Shutdown
  , openGL2NewFrame
  , openGL2RenderDrawData
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
C.include "backends/imgui_impl_opengl2.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplOpenGL2_Init@.
openGL2Init :: MonadIO m => m Bool
openGL2Init = ( 0 /= ) <$> liftIO do
  [C.exp| bool { ImGui_ImplOpenGL2_Init() } |]


-- | Wraps @ImGui_ImplOpenGL2_Shutdown@.
openGL2Shutdown :: MonadIO m => m ()
openGL2Shutdown = liftIO do
  [C.exp| void { ImGui_ImplOpenGL2_Shutdown(); } |]


-- | Wraps @ImGui_ImplOpenGL2_NewFrame@.
openGL2NewFrame :: MonadIO m => m ()
openGL2NewFrame = liftIO do
  [C.exp| void { ImGui_ImplOpenGL2_NewFrame(); } |]


-- | Wraps @ImGui_ImplOpenGL2_RenderDrawData@.
openGL2RenderDrawData :: MonadIO m => DrawData -> m ()
openGL2RenderDrawData (DrawData ptr) = liftIO do
  [C.exp| void { ImGui_ImplOpenGL2_RenderDrawData((ImDrawData*) $( void* ptr )) } |]
