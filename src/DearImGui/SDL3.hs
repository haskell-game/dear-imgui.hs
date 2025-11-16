{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module: DearImGui.SDL

SDL3 specific functions backend for Dear ImGui.

Modules for initialising a backend with SDL3 can be found under the corresponding backend,
e.g. "DearImGui.SDL.OpenGL".
-}
module DearImGui.SDL3 (
  sdl3NewFrame,
  sdl3Shutdown,
  pollEventWithImGui,
  pollEventsWithImGui,
)
where

-- , dispatchRawEvent

-- base
import Control.Monad (
  void,
  when,
 )
import Foreign.Marshal.Alloc (
  alloca,
 )
import Foreign.Ptr (
  Ptr,
  castPtr,
 )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- sdl3

-- transformers
import Control.Monad.IO.Class (
  MonadIO,
  liftIO,
 )
import SDL.Events

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_sdl3.h"
C.include "SDL3/SDL.h"
Cpp.using "namespace ImGui"

-- | Wraps @ImGui_ImplSDL3_NewFrame@.
sdl3NewFrame :: (MonadIO m) => m ()
sdl3NewFrame = liftIO do
  [C.exp| void { ImGui_ImplSDL3_NewFrame(); } |]

-- | Wraps @ImGui_ImplSDL3_Shutdown@.
sdl3Shutdown :: (MonadIO m) => m ()
sdl3Shutdown = liftIO do
  [C.exp| void { ImGui_ImplSDL3_Shutdown(); } |]

{- | Call the SDL3 'pollEvent' function, while also dispatching the event to
Dear ImGui. You should use this in your application instead of 'pollEvent'.
-}
pollEventWithImGui :: (MonadIO m) => m (Maybe SDLEvent)
pollEventWithImGui = liftIO do
  alloca \evPtr -> do
    sdlPumpEvents

    -- We use NULL first to check if there's an event.
    nEvents <- sdlPeepEventsRaw evPtr 1 SDL_PEEKEVENT SDL_EVENT_FIRST SDL_EVENT_LAST

    when (nEvents > 0) do
      void $ dispatchRawEvent evPtr

    sdlPollEvent

{- | Dispatch a raw 'Raw.Event' value to Dear ImGui.

You may want this function instead of 'pollEventWithImGui' if you do not use
@sdl3@'s higher-level 'Event' type (e.g. your application has its own polling
mechanism).

__It is your application's responsibility to both manage the input__
__pointer's memory and to fill the memory location with a raw 'Raw.Event'__
__value.__
-}
dispatchRawEvent :: (MonadIO m) => Ptr SDLEvent -> m Bool
dispatchRawEvent evPtr = liftIO do
  let evPtr' = castPtr evPtr :: Ptr ()
  (0 /=) <$> [C.exp| bool { ImGui_ImplSDL3_ProcessEvent((const SDL_Event*) $(void* evPtr')) } |]

{- | Like the SDL3 'pollEvents' function, while also dispatching the events to
Dear ImGui. See 'pollEventWithImGui'.
-}
pollEventsWithImGui :: (MonadIO m) => m [SDLEvent]
pollEventsWithImGui = do
  e <- pollEventWithImGui
  case e of
    Nothing -> pure []
    Just e' -> (e' :) <$> pollEventsWithImGui