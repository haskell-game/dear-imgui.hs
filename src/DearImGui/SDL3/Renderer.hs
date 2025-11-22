{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module: DearImGUI.SDL.Renderer

Initialising the SDL2 renderer backend for Dear ImGui.
-}
module DearImGui.SDL3.Renderer (
    sdl3InitForSDLRenderer,
    sdl3RendererInit,
    sdl3RendererShutdown,
    sdl3RendererNewFrame,
    sdl3RendererRenderDrawData,
)
where

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- sdl2
-- import SDL.Internal.Types (
--     Renderer (..),
--     Window (..),
--  )

-- transformers
import Control.Monad.IO.Class (
    MonadIO,
    liftIO,
 )

-- DearImGui
import DearImGui (
    DrawData (..),
 )
import Foreign.Ptr
import SDL (SDLRenderer (SDLRenderer), SDLWindow (SDLWindow))

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_sdlrenderer3.h"
C.include "backends/imgui_impl_sdl3.h"
C.include "SDL3/SDL.h"
Cpp.using "namespace ImGui"

-- | Wraps @ImGui_ImplSDL3_InitForSDLRenderer@.
sdl3InitForSDLRenderer :: (MonadIO m) => SDLWindow -> SDLRenderer -> m Bool
sdl3InitForSDLRenderer (SDLWindow windowPtr) (SDLRenderer renderPtr) = liftIO do
    let windowPtr' = castPtr windowPtr :: Ptr ()
        renderPtr' = castPtr renderPtr :: Ptr ()
    (0 /=) <$> [C.exp| bool { ImGui_ImplSDL3_InitForSDLRenderer((SDL_Window*)$(void* windowPtr'), (SDL_Renderer*)$(void* renderPtr')) } |]

-- | Wraps @ImGui_ImplSDLRenderer3_Init@.
sdl3RendererInit :: (MonadIO m) => SDLRenderer -> m Bool
sdl3RendererInit (SDLRenderer renderPtr) = liftIO do
    let renderPtr' = castPtr renderPtr :: Ptr ()
    (0 /=) <$> [C.exp| bool { ImGui_ImplSDLRenderer3_Init((SDL_Renderer*)$(void* renderPtr')) } |]

-- | Wraps @ImGui_ImplSDLRenderer3_Shutdown@.
sdl3RendererShutdown :: (MonadIO m) => m ()
sdl3RendererShutdown = liftIO do
    [C.exp| void { ImGui_ImplSDLRenderer3_Shutdown(); } |]

-- | Wraps @ImGui_ImplSDLRenderer3_NewFrame@.
sdl3RendererNewFrame :: (MonadIO m) => m ()
sdl3RendererNewFrame = liftIO do
    [C.exp| void { ImGui_ImplSDLRenderer3_NewFrame(); } |]

-- | Wraps @ImGui_ImplSDLRenderer3_RenderDrawData@.
sdl3RendererRenderDrawData :: (MonadIO m) => SDLRenderer -> DrawData -> m ()
sdl3RendererRenderDrawData (SDLRenderer renderPtr) (DrawData ptr) = liftIO do
    let renderPtr' = castPtr renderPtr :: Ptr ()
    [C.exp| void { ImGui_ImplSDLRenderer3_RenderDrawData((ImDrawData*) $( void* ptr ), (SDL_Renderer*) $( void* renderPtr' )) } |]
