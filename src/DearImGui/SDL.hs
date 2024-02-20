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
    -- *** Raw
  , dispatchRawEvent
  )
  where

-- base
import Control.Monad
  ( void, when )
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
import qualified SDL.Raw.Types as Raw

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )


C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_sdl2.h"
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
      void $ dispatchRawEvent evPtr

    pollEvent

-- | Dispatch a raw 'Raw.Event' value to Dear ImGui.
--
-- You may want this function instead of 'pollEventWithImGui' if you do not use
-- @sdl2@'s higher-level 'Event' type (e.g. your application has its own polling
-- mechanism).
--
-- __It is your application's responsibility to both manage the input__
-- __pointer's memory and to fill the memory location with a raw 'Raw.Event'__
-- __value.__
dispatchRawEvent :: MonadIO m => Ptr Raw.Event -> m Bool
dispatchRawEvent evPtr = liftIO do
  let evPtr' = castPtr evPtr :: Ptr ()
  (0 /=) <$> [C.exp| bool { ImGui_ImplSDL2_ProcessEvent((const SDL_Event*) $(void* evPtr')) } |]

-- | Like the SDL2 'pollEvents' function, while also dispatching the events to
-- Dear ImGui. See 'pollEventWithImGui'.
pollEventsWithImGui :: MonadIO m => m [Event]
pollEventsWithImGui = do
  e <- pollEventWithImGui
  case e of
    Nothing -> pure []
    Just e' -> ( e' : ) <$> pollEventsWithImGui
