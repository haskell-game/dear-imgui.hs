{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.SDL

SDL2 specific functions backend for Dear ImGui.

Modules for initialising a backend with SDL2 can be found under the corresponding backend,
e.g. "DearImGui.SDL.OpenGL".
-}

module DearImGui.SDL (
    -- ** SDL2
    sdl2NewFrame
  , sdl2Shutdown
  , pollEventWithImGui
  , pollEventsWithImGui
  )
  where

-- base
import Control.Monad
  ( when )
import Foreign.Marshal.Alloc
  ( alloca )
import Foreign.Ptr
  ( Ptr, castPtr )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- sdl2
import SDL
import SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )


C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_sdl.h"
C.include "SDL.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGui_ImplSDL2_NewFrame@.
sdl2NewFrame :: MonadIO m => m ()
sdl2NewFrame = liftIO do
  [C.exp| void { ImGui_ImplSDL2_NewFrame(); } |]


-- | Wraps @ImGui_ImplSDL2_Shutdown@.
sdl2Shutdown :: MonadIO m => m ()
sdl2Shutdown = liftIO do
  [C.exp| void { ImGui_ImplSDL2_Shutdown(); } |]

-- | Call the SDL2 'pollEvent' function, while also dispatching the event to
-- Dear ImGui. You should use this in your application instead of 'pollEvent'.
pollEventWithImGui :: MonadIO m => m (Maybe Event)
pollEventWithImGui = liftIO do
  alloca \evPtr -> do
    pumpEvents

    -- We use NULL first to check if there's an event.
    nEvents <- Raw.peepEvents evPtr 1 Raw.SDL_PEEKEVENT Raw.SDL_FIRSTEVENT Raw.SDL_LASTEVENT

    when (nEvents > 0) do
      let evPtr' = castPtr evPtr :: Ptr ()
      [C.exp| void { ImGui_ImplSDL2_ProcessEvent((SDL_Event*) $(void* evPtr')) } |]

    pollEvent

-- | Like the SDL2 'pollEvents' function, while also dispatching the events to
-- Dear ImGui. See 'pollEventWithImGui'.
pollEventsWithImGui :: MonadIO m => m [Event]
pollEventsWithImGui = do
  e <- pollEventWithImGui
  case e of
    Nothing -> pure []
    Just e' -> ( e' : ) <$> pollEventsWithImGui
