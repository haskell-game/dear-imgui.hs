{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGUI.SDL.Renderer

Initialising the SDL2 renderer backend for Dear ImGui.
-}

module DearImGui.SDL.Renderer
  ( sdl2InitForSDLRenderer
  , sdlRendererInit
  , sdlRendererShutdown
  , sdlRendererNewFrame
  , sdlRendererRenderDrawData
  )
  where

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- sdl2
import SDL.Internal.Types
  ( Renderer(..), Window(..) )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- DearImGui
import DearImGui
  ( DrawData(..) )


C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_sdlrenderer2.h"
C.include "backends/imgui_impl_sdl2.h"
C.include "SDL.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplSDL2_InitForSDLRenderer@.
sdl2InitForSDLRenderer :: MonadIO m => Window -> Renderer -> m Bool
sdl2InitForSDLRenderer (Window windowPtr) (Renderer renderPtr) = liftIO do
  (0 /=) <$> [C.exp| bool { ImGui_ImplSDL2_InitForSDLRenderer((SDL_Window*)$(void* windowPtr), (SDL_Renderer*)$(void* renderPtr)) } |]

-- | Wraps @ImGui_ImplSDLRenderer2_Init@.
sdlRendererInit :: MonadIO m => Renderer -> m Bool
sdlRendererInit (Renderer renderPtr) = liftIO do
  (0 /=) <$> [C.exp| bool { ImGui_ImplSDLRenderer2_Init((SDL_Renderer*)$(void* renderPtr)) } |]

-- | Wraps @ImGui_ImplSDLRenderer2_Shutdown@.
sdlRendererShutdown :: MonadIO m => m ()
sdlRendererShutdown = liftIO do
  [C.exp| void { ImGui_ImplSDLRenderer2_Shutdown(); } |]

-- | Wraps @ImGui_ImplSDLRenderer2_NewFrame@.
sdlRendererNewFrame :: MonadIO m => m ()
sdlRendererNewFrame = liftIO do
  [C.exp| void { ImGui_ImplSDLRenderer2_NewFrame(); } |]

-- | Wraps @ImGui_ImplSDLRenderer2_RenderDrawData@.
sdlRendererRenderDrawData :: MonadIO m => DrawData -> m ()
sdlRendererRenderDrawData (DrawData ptr) = liftIO do
  [C.exp| void { ImGui_ImplSDLRenderer2_RenderDrawData((ImDrawData*) $( void* ptr )) } |]
