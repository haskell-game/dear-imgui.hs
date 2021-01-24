{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DearImGui
  ( -- * Context Creation and Access
    Context(..)
  , createContext
  , destroyContext

    -- * Main
  , newFrame
  , endFrame
  , render
  , DrawData(..)
  , getDrawData
  , checkVersion

    -- ** SDL2
  , sdl2InitForOpenGL
  , sdl2NewFrame
  , sdl2Shutdown
  , pollEventWithImGui

    -- ** OpenGL 2
  , openGL2Init
  , openGL2Shutdown
  , openGL2NewFrame
  , openGL2RenderDrawData

    -- * Demo, Debug, Information
  , showDemoWindow
  , showMetricsWindow
  , showAboutWindow
  , showUserGuide
  , getVersion

    -- * Styles
  , styleColorsDark
  , styleColorsLight
  , styleColorsClassic

    -- * Windows
  , begin
  , end

    -- * Widgets
    -- ** Text
  , text

    -- ** Main
  , button
  , smallButton
  , arrowButton
  , checkbox

    -- * Types
  , ImGuiDir
  , pattern ImGuiDirLeft
  , pattern ImGuiDirRight
  , pattern ImGuiDirUp
  , pattern ImGuiDirDown
  )
  where

import Data.Bool
import Data.StateVar
import Control.Monad ( when )
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import SDL
import SDL.Internal.Types
import SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw
import Unsafe.Coerce ( unsafeCoerce )

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_opengl2.h"
C.include "backends/imgui_impl_sdl.h"
C.include "SDL.h"
C.include "SDL_opengl.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGuiContext*@.
newtype Context = Context (Ptr ())


-- | Wraps @ImGui::CreateContext()@.
createContext :: IO Context
createContext =
  Context <$> [C.exp| void* { CreateContext() } |]


-- | Wraps @ImGui::DestroyContext()@.
destroyContext :: Context -> IO ()
destroyContext (Context contextPtr) =
  [C.exp| void { DestroyContext((ImGuiContext*)$(void* contextPtr)); } |]


-- | Start a new Dear ImGui frame, you can submit any command from this point
-- until 'render'/'endFrame'.
--
-- Wraps @ImGui::NewFrame()@.
newFrame :: IO ()
newFrame = [C.exp| void { ImGui::NewFrame(); } |]


-- | Ends the Dear ImGui frame. automatically called by 'render'. If you don't
-- need to render data (skipping rendering) you may call 'endFrame' without
-- 'render'... but you'll have wasted CPU already! If you don't need to render,
-- better to not create any windows and not call 'newFrame' at all!
endFrame :: IO ()
endFrame = [C.exp| void { ImGui::EndFrame(); } |]


-- | Ends the Dear ImGui frame, finalize the draw data. You can then get call
-- 'getDrawData'.
render :: IO ()
render = [C.exp| void { ImGui::Render(); } |]


-- | Wraps @ImDrawData*@.
newtype DrawData = DrawData (Ptr ())


-- | Valid after 'render' and until the next call to 'newFrame'. This is what
-- you have to render.
getDrawData :: IO DrawData
getDrawData = DrawData <$> [C.exp| void* { ImGui::GetDrawData() } |]


-- | Wraps @IMGUI_CHECKVERSION()@
checkVersion :: IO ()
checkVersion =
  [C.exp| void { IMGUI_CHECKVERSION(); } |]


-- | Wraps @ImGui_ImplSDL2_InitForOpenGL@.
sdl2InitForOpenGL :: Window -> GLContext -> IO ()
sdl2InitForOpenGL (Window windowPtr) glContext =
  [C.exp| void { ImGui_ImplSDL2_InitForOpenGL((SDL_Window*)$(void* windowPtr), $(void* glContextPtr)); } |]
  where
    glContextPtr :: Ptr ()
    glContextPtr = unsafeCoerce glContext


-- | Wraps @ImGui_ImplSDL2_NewFrame@.
sdl2NewFrame :: Window -> IO ()
sdl2NewFrame (Window windowPtr) =
  [C.exp| void { ImGui_ImplSDL2_NewFrame((SDL_Window*)($(void* windowPtr))); } |]


-- | Wraps @ImGui_ImplSDL2_Shutdown@.
sdl2Shutdown :: IO ()
sdl2Shutdown = [C.exp| void { ImGui_ImplSDL2_Shutdown(); } |]


-- | Call the SDL2 'pollEvent' function, while also dispatching the event to
-- Dear ImGui. You should use this in your application instead of 'pollEvent'.
pollEventWithImGui :: IO (Maybe Event)
pollEventWithImGui = alloca \evPtr -> do
  pumpEvents

  -- We use NULL first to check if there's an event.
  nEvents <- Raw.peepEvents evPtr 1 Raw.SDL_PEEKEVENT Raw.SDL_FIRSTEVENT Raw.SDL_LASTEVENT

  when (nEvents > 0) do
    let evPtr' = castPtr evPtr :: Ptr ()
    [C.exp| void { ImGui_ImplSDL2_ProcessEvent((SDL_Event*) $(void* evPtr')) } |]

  pollEvent


-- | Wraps @ImGui_ImplOpenGL2_Init@.
openGL2Init :: IO ()
openGL2Init = [C.exp| void { ImGui_ImplOpenGL2_Init(); } |]


-- | Wraps @ImGui_ImplOpenGL2_Shutdown@.
openGL2Shutdown :: IO ()
openGL2Shutdown = [C.exp| void { ImGui_ImplOpenGL2_Shutdown(); } |]


-- | Wraps @ImGui_ImplOpenGL2_NewFrame@.
openGL2NewFrame :: IO ()
openGL2NewFrame = [C.exp| void { ImGui_ImplOpenGL2_NewFrame(); } |]


-- | Wraps @ImGui_ImplOpenGL2_RenderDrawData@.
openGL2RenderDrawData :: DrawData -> IO ()
openGL2RenderDrawData (DrawData ptr) = [C.exp| void { ImGui_ImplOpenGL2_RenderDrawData((ImDrawData*) $( void* ptr )) } |]


-- | Create demo window. Demonstrate most ImGui features. Call this to learn
-- about the library! Try to make it always available in your application!
showDemoWindow :: IO ()
showDemoWindow = [C.exp| void { ImGui::ShowDemoWindow(); } |]


-- | Create Metrics/Debugger window. Display Dear ImGui internals: windows, draw
-- commands, various internal state, etc.
showMetricsWindow :: IO ()
showMetricsWindow = [C.exp| void { ImGui::ShowMetricsWindow(); } |]


-- | Create About window. display Dear ImGui version, credits and build/system
-- information.
showAboutWindow :: IO ()
showAboutWindow = [C.exp| void { ShowAboutWindow(); } |]


-- | Add basic help/info block (not a window): how to manipulate ImGui as a
-- end-user (mouse/keyboard controls).
showUserGuide :: IO ()
showUserGuide = [C.exp| void { ShowUserGuide() } |]


-- | Get the compiled version string e.g. "1.80 WIP" (essentially the value for
-- @IMGUI_VERSION@ from the compiled version of @imgui.cpp@).
getVersion :: IO String
getVersion = peekCString =<< [C.exp| const char* { GetVersion() } |]


-- | New, recommended style (default).
--
-- Wraps @ImGui::StyleColorsDark()@.
styleColorsDark :: IO ()
styleColorsDark = [C.exp| void { StyleColorsDark(); } |]


-- | Best used with borders and a custom, thicker font.
--
-- Wraps @ImGui::StyleColorsLight()@.
styleColorsLight :: IO ()
styleColorsLight = [C.exp| void { StyleColorsLight(); } |]


-- | Classic ImGui style.
--
-- Wraps @ImGui::StyleColorsClasic()@.
styleColorsClassic :: IO ()
styleColorsClassic = [C.exp| void { StyleColorsClassic(); } |]


-- | Push window to the stack and start appending to it.
--
-- Returns 'False' to indicate the window is collapsed or fully clipped, so you
-- may early out and omit submitting anything to the window. Always call a
-- matching 'end' for each 'begin' call, regardless of its return value!
--
-- Wraps @ImGui::Begin()@.
begin :: String -> IO Bool
begin name = withCString name \namePtr ->
  (1 ==) <$> [C.exp| bool { ImGui::Begin($(char* namePtr)) } |]


-- | Pop window from the stack.
--
-- Wraps @ImGui::End()@.
end :: IO ()
end = [C.exp| void { ImGui::End(); } |]


-- | Formatted text.
--
-- Wraps @ImGui::Text()@.
text :: String -> IO ()
text t = withCString t \textPtr ->
  [C.exp| void { Text($(char* textPtr)) } |]


-- | A button. Returns 'True' when clicked.
--
-- Wraps @ImGui::Button()@.
button :: String -> IO Bool
button label = withCString label \labelPtr ->
  (1 ==) <$> [C.exp| bool { Button($(char* labelPtr)) } |]


-- | Button with @FramePadding=(0,0)@ to easily embed within text.
--
-- Wraps @ImGui::SmallButton()@.
smallButton :: String -> IO Bool
smallButton label = withCString label \labelPtr ->
  (1 ==) <$> [C.exp| bool { SmallButton($(char* labelPtr)) } |]


-- | Square button with an arrow shape.
--
-- Wraps @ImGui::ArrowButton()@.
arrowButton :: String -> ImGuiDir -> IO Bool
arrowButton strId (ImGuiDir dir) = withCString strId \strIdPtr ->
  (1 ==) <$> [C.exp| bool { ArrowButton($(char* strIdPtr), $(int dir)) } |]


-- | Wraps @ImGui::Checkbox()@.
checkbox :: (HasSetter ref Bool, HasGetter ref Bool) => String -> ref -> IO Bool
checkbox label ref = do
  currentValue <- get ref
  with (bool 0 1 currentValue :: CBool) \boolPtr -> do
    changed <- withCString label \labelPtr ->
      (1 ==) <$> [C.exp| bool { Checkbox($(char* labelPtr), $(bool* boolPtr)) } |]

    newValue <- peek boolPtr
    ref $=! (newValue == 1)

    return changed


-- | A cardinal direction.
newtype ImGuiDir      = ImGuiDir CInt


pattern ImGuiDirLeft, ImGuiDirRight, ImGuiDirUp, ImGuiDirDown :: ImGuiDir
pattern ImGuiDirLeft  = ImGuiDir 0
pattern ImGuiDirRight = ImGuiDir 1
pattern ImGuiDirUp    = ImGuiDir 2
pattern ImGuiDirDown  = ImGuiDir 3
