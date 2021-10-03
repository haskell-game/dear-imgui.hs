{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-| Main configuration and I/O between your application and ImGui

-}

module DearImGui.Raw.IO where

-- TODO: add exports

import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign
  ( Ptr )
import Foreign.C
  ( CFloat(..)
  , CString
  )

-- dear-imgui
import DearImGui.Context
  ( imguiContext )
-- import DearImGui.Enums
-- import DearImGui.Structs

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"

setIniFilename :: MonadIO m => CString -> m ()
setIniFilename ptr = liftIO do
  [C.block|
    void {
      GetIO().IniFilename = $(char * ptr);
    }
  |]

setLogFilename :: MonadIO m => CString -> m ()
setLogFilename ptr = liftIO do
  [C.block|
    void {
      GetIO().LogFilename = $(char * ptr);
    }
  |]

setMouseDoubleClickTime :: MonadIO m => CFloat -> m ()
setMouseDoubleClickTime seconds = liftIO do
  [C.block|
    void {
      GetIO().MouseDoubleClickTime = $(float seconds);
    }
  |]

setMouseDoubleClickMaxDist :: MonadIO m => CFloat -> m ()
setMouseDoubleClickMaxDist pixels = liftIO do
  [C.block|
    void {
      GetIO().MouseDoubleClickMaxDist = $(float pixels);
    }
  |]

setMouseDragThreshold :: MonadIO m => CFloat -> m ()
setMouseDragThreshold pixels = liftIO do
  [C.block|
    void {
      GetIO().MouseDragThreshold = $(float pixels);
    }
  |]

setKeyRepeatDelay :: MonadIO m => CFloat -> m ()
setKeyRepeatDelay seconds = liftIO do
  [C.block|
    void {
      GetIO().KeyRepeatDelay = $(float seconds);
    }
  |]

setKeyRepeatRate :: MonadIO m => CFloat -> m ()
setKeyRepeatRate pixels = liftIO do
  [C.block|
    void {
      GetIO().KeyRepeatRate = $(float pixels);
    }
  |]

setUserData :: MonadIO m => Ptr () -> m ()
setUserData ptr = liftIO do
  [C.block|
    void {
      GetIO().UserData = $(void* ptr);
    }
  |]

{- TODO:

bool   WantCaptureMouse;         // Set when Dear ImGui will use mouse inputs, in this case do not dispatch them to your main game/application (either way, always pass on mouse inputs to imgui). (e.g. unclicked mouse is hovering over an imgui window, widget is active, mouse was clicked over an imgui window, etc.).
bool   WantCaptureKeyboard;      // Set when Dear ImGui will use keyboard inputs, in this case do not dispatch them to your main game/application (either way, always pass keyboard inputs to imgui). (e.g. InputText active, or an imgui window is focused and navigation is enabled, etc.).
bool   WantTextInput;            // Mobile/console: when set, you may display an on-screen keyboard. This is set by Dear ImGui when it wants textual keyboard input to happen (e.g. when a InputText widget is active).
bool   WantSetMousePos;          // MousePos has been altered, backend should reposition mouse on next frame. Rarely used! Set only when ImGuiConfigFlags_NavEnableSetMousePos flag is enabled.
bool   WantSaveIniSettings;      // When manual .ini load/save is active (io.IniFilename == NULL), this will be set to notify your application that you can call SaveIniSettingsToMemory() and save yourself. Important: clear io.WantSaveIniSettings yourself after saving!
bool   NavActive;                // Keyboard/Gamepad navigation is currently allowed (will handle ImGuiKey_NavXXX events) = a window is focused and it doesn't use the ImGuiWindowFlags_NoNavInputs flag.
bool   NavVisible;               // Keyboard/Gamepad navigation is visible and allowed (will handle ImGuiKey_NavXXX events).
float  Framerate;                // Rough estimate of application framerate, in frame per second. Solely for convenience. Rolling average estimation based on io.DeltaTime over 120 frames.
int    MetricsRenderVertices;    // Vertices output during last call to Render()
int    MetricsRenderIndices;     // Indices output during last call to Render() = number of triangles * 3
int    MetricsRenderWindows;     // Number of visible windows
int    MetricsActiveWindows;     // Number of active windows
int    MetricsActiveAllocations; // Number of active allocations, updated by MemAlloc/MemFree based on current context. May be off if you have multiple imgui contexts.
ImVec2 MouseDelta;
-}
