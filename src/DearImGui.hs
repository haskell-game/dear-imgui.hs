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

    -- * Cursor/Layout
  , separator
  , sameLine

    -- * Widgets
    -- ** Text
  , text

    -- ** Main
  , button
  , smallButton
  , arrowButton
  , checkbox
  , progressBar
  , bullet

    -- ** Combo Box
  , beginCombo
  , endCombo

    -- ** Selectables
  , selectable

    -- ** Menus
  , beginMenuBar
  , endMenuBar
  , beginMainMenuBar
  , endMainMenuBar
  , beginMenu
  , endMenu
  , menuItem

    -- * Types
  , ImGuiDir
  , pattern ImGuiDirLeft
  , pattern ImGuiDirRight
  , pattern ImGuiDirUp
  , pattern ImGuiDirDown
  )
  where

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bool
import Data.StateVar
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
createContext :: MonadIO m => m Context
createContext = liftIO do
  Context <$> [C.exp| void* { CreateContext() } |]


-- | Wraps @ImGui::DestroyContext()@.
destroyContext :: MonadIO m => Context -> m ()
destroyContext (Context contextPtr) = liftIO do
  [C.exp| void { DestroyContext((ImGuiContext*)$(void* contextPtr)); } |]


-- | Start a new Dear ImGui frame, you can submit any command from this point
-- until 'render'/'endFrame'.
--
-- Wraps @ImGui::NewFrame()@.
newFrame :: MonadIO m => m ()
newFrame = liftIO do
  [C.exp| void { ImGui::NewFrame(); } |]


-- | Ends the Dear ImGui frame. automatically called by 'render'. If you don't
-- need to render data (skipping rendering) you may call 'endFrame' without
-- 'render'... but you'll have wasted CPU already! If you don't need to render,
-- better to not create any windows and not call 'newFrame' at all!
endFrame :: MonadIO m => m ()
endFrame = liftIO do
  [C.exp| void { ImGui::EndFrame(); } |]


-- | Ends the Dear ImGui frame, finalize the draw data. You can then get call
-- 'getDrawData'.
render :: MonadIO m => m ()
render = liftIO do
  [C.exp| void { ImGui::Render(); } |]


-- | Wraps @ImDrawData*@.
newtype DrawData = DrawData (Ptr ())


-- | Valid after 'render' and until the next call to 'newFrame'. This is what
-- you have to render.
getDrawData :: MonadIO m => m DrawData
getDrawData = liftIO do
  DrawData <$> [C.exp| void* { ImGui::GetDrawData() } |]


-- | Wraps @IMGUI_CHECKVERSION()@
checkVersion :: MonadIO m => m ()
checkVersion = liftIO do
  [C.exp| void { IMGUI_CHECKVERSION(); } |]


-- | Wraps @ImGui_ImplSDL2_InitForOpenGL@.
sdl2InitForOpenGL :: MonadIO m => Window -> GLContext -> m ()
sdl2InitForOpenGL (Window windowPtr) glContext = liftIO do
  [C.exp| void { ImGui_ImplSDL2_InitForOpenGL((SDL_Window*)$(void* windowPtr), $(void* glContextPtr)); } |]
  where
    glContextPtr :: Ptr ()
    glContextPtr = unsafeCoerce glContext


-- | Wraps @ImGui_ImplSDL2_NewFrame@.
sdl2NewFrame :: MonadIO m => Window -> m ()
sdl2NewFrame (Window windowPtr) = liftIO do
  [C.exp| void { ImGui_ImplSDL2_NewFrame((SDL_Window*)($(void* windowPtr))); } |]


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


-- | Wraps @ImGui_ImplOpenGL2_Init@.
openGL2Init :: MonadIO m => m ()
openGL2Init = liftIO do
  [C.exp| void { ImGui_ImplOpenGL2_Init(); } |]


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


-- | Create demo window. Demonstrate most ImGui features. Call this to learn
-- about the library! Try to make it always available in your application!
showDemoWindow :: MonadIO m => m ()
showDemoWindow = liftIO do
  [C.exp| void { ImGui::ShowDemoWindow(); } |]


-- | Create Metrics/Debugger window. Display Dear ImGui internals: windows, draw
-- commands, various internal state, etc.
showMetricsWindow :: MonadIO m => m ()
showMetricsWindow = liftIO do
  [C.exp| void { ImGui::ShowMetricsWindow(); } |]


-- | Create About window. display Dear ImGui version, credits and build/system
-- information.
showAboutWindow :: MonadIO m => m ()
showAboutWindow = liftIO do
  [C.exp| void { ShowAboutWindow(); } |]


-- | Add basic help/info block (not a window): how to manipulate ImGui as a
-- end-user (mouse/keyboard controls).
showUserGuide :: MonadIO m => m ()
showUserGuide = liftIO do
  [C.exp| void { ShowUserGuide() } |]


-- | Get the compiled version string e.g. "1.80 WIP" (essentially the value for
-- @IMGUI_VERSION@ from the compiled version of @imgui.cpp@).
getVersion :: MonadIO m => m String
getVersion = liftIO do
  peekCString =<< [C.exp| const char* { GetVersion() } |]


-- | New, recommended style (default).
--
-- Wraps @ImGui::StyleColorsDark()@.
styleColorsDark :: MonadIO m => m ()
styleColorsDark = liftIO do
  [C.exp| void { StyleColorsDark(); } |]


-- | Best used with borders and a custom, thicker font.
--
-- Wraps @ImGui::StyleColorsLight()@.
styleColorsLight :: MonadIO m => m ()
styleColorsLight = liftIO do
  [C.exp| void { StyleColorsLight(); } |]


-- | Classic ImGui style.
--
-- Wraps @ImGui::StyleColorsClasic()@.
styleColorsClassic :: MonadIO m => m ()
styleColorsClassic = liftIO do
  [C.exp| void { StyleColorsClassic(); } |]


-- | Push window to the stack and start appending to it.
--
-- Returns 'False' to indicate the window is collapsed or fully clipped, so you
-- may early out and omit submitting anything to the window. Always call a
-- matching 'end' for each 'begin' call, regardless of its return value!
--
-- Wraps @ImGui::Begin()@.
begin :: MonadIO m => String -> m Bool
begin name = liftIO do
  withCString name \namePtr ->
    (1 ==) <$> [C.exp| bool { ImGui::Begin($(char* namePtr)) } |]


-- | Pop window from the stack.
--
-- Wraps @ImGui::End()@.
end :: MonadIO m => m ()
end = liftIO do
  [C.exp| void { ImGui::End(); } |]


-- | Separator, generally horizontal. inside a menu bar or in horizontal layout
-- mode, this becomes a vertical separator.
--
-- Wraps @ImGui::Separator()@
separator :: MonadIO m => m ()
separator = liftIO do
  [C.exp| void { Separator(); } |]


-- | Call between widgets or groups to layout them horizontally.
--
-- Wraps @ImGui::SameLine@.
sameLine :: MonadIO m => m ()
sameLine = liftIO do
  [C.exp| void { SameLine(); } |]


-- | Formatted text.
--
-- Wraps @ImGui::Text()@.
text :: MonadIO m => String -> m ()
text t = liftIO do
  withCString t \textPtr ->
    [C.exp| void { Text($(char* textPtr)) } |]


-- | A button. Returns 'True' when clicked.
--
-- Wraps @ImGui::Button()@.
button :: MonadIO m => String -> m Bool
button label = liftIO do
  withCString label \labelPtr ->
    (1 ==) <$> [C.exp| bool { Button($(char* labelPtr)) } |]


-- | Button with @FramePadding=(0,0)@ to easily embed within text.
--
-- Wraps @ImGui::SmallButton()@.
smallButton :: MonadIO m => String -> m Bool
smallButton label = liftIO do
  withCString label \labelPtr ->
    (1 ==) <$> [C.exp| bool { SmallButton($(char* labelPtr)) } |]


-- | Square button with an arrow shape.
--
-- Wraps @ImGui::ArrowButton()@.
arrowButton :: MonadIO m => String -> ImGuiDir -> m Bool
arrowButton strId (ImGuiDir dir) = liftIO do
  withCString strId \strIdPtr ->
    (1 ==) <$> [C.exp| bool { ArrowButton($(char* strIdPtr), $(int dir)) } |]


-- | Wraps @ImGui::Checkbox()@.
checkbox :: (HasSetter ref Bool, HasGetter ref Bool, MonadIO m) => String -> ref -> m Bool
checkbox label ref = liftIO do
  currentValue <- get ref
  with (bool 0 1 currentValue :: CBool) \boolPtr -> do
    changed <- withCString label \labelPtr ->
      (1 ==) <$> [C.exp| bool { Checkbox($(char* labelPtr), $(bool* boolPtr)) } |]

    newValue <- peek boolPtr
    ref $=! (newValue == 1)

    return changed


progressBar :: MonadIO m => Float -> Maybe String -> m ()
progressBar progress overlay = liftIO do
  withCStringOrNull overlay \overlayPtr ->
    [C.exp| void { ProgressBar($(float c'progress), ImVec2(-FLT_MIN, 0), $(char* overlayPtr)) } |]
  where
    c'progress :: CFloat
    c'progress = realToFrac progress


-- | Draw a small circle + keep the cursor on the same line. Advance cursor x
-- position by 'getTreeNodeToLabelSpacing', same distance that 'treeNode' uses.
bullet :: MonadIO m => m ()
bullet = liftIO do
  [C.exp| void { Bullet() } |]


-- | Begin creating a combo box with a given label and preview value.
--
-- Returns 'True' if the combo box is open. In this state, you should populate
-- the contents of the combo box - for example, by calling 'selectable'.
--
-- Wraps @ImGui::BeginCombo()@.
beginCombo :: MonadIO m => String -> String -> m Bool
beginCombo label previewValue = liftIO $
  withCString label        \labelPtr ->
  withCString previewValue \previewValuePtr ->
  (1 ==) <$> [C.exp| bool { BeginCombo($(char* labelPtr), $(char* previewValuePtr)) } |]


-- | Only call 'endCombo' if 'beginCombon' returns 'True'!
--
-- Wraps @ImGui::EndCombo()@.
endCombo :: MonadIO m => m ()
endCombo = liftIO do
  [C.exp| void { EndCombo() } |]


-- | Wraps @ImGui::Selectable()@.
selectable :: MonadIO m => String -> m Bool
selectable label = liftIO do
  withCString label \labelPtr ->
    (1 == ) <$> [C.exp| bool { Selectable($(char* labelPtr)) } |]


-- | Append to menu-bar of current window (requires 'ImGuiWindowFlagsMenuBar'
-- flag set on parent window).
--
-- Wraps @ImGui::BeginMenuBar()@.
beginMenuBar :: MonadIO m => m Bool
beginMenuBar = liftIO do
  (1 == ) <$> [C.exp| bool { BeginMenuBar() } |]


-- | Only call 'endMenuBar' if 'beginMenuBar' returns true!
--
-- Wraps @ImGui::EndMenuBar()@.
endMenuBar :: MonadIO m => m ()
endMenuBar = liftIO do
  [C.exp| void { EndMenuBar(); } |]


-- | Create and append to a full screen menu-bar.
--
-- Wraps @ImGui::BeginMainMenuBar()@.
beginMainMenuBar :: MonadIO m => m Bool
beginMainMenuBar = liftIO do
  (1 == ) <$> [C.exp| bool { BeginMainMenuBar() } |]


-- | Only call 'endMainMenuBar' if 'beginMainMenuBar' returns true!
--
-- Wraps @ImGui::EndMainMenuBar()@.
endMainMenuBar :: MonadIO m => m ()
endMainMenuBar = liftIO do
  [C.exp| void { EndMainMenuBar(); } |]


-- | Create a sub-menu entry.
--
-- Wraps @ImGui::BeginMenu()@.
beginMenu :: MonadIO m => String -> m Bool
beginMenu label = liftIO do
  withCString label \labelPtr ->
    (1 == ) <$> [C.exp| bool { BeginMenu($(char* labelPtr)) } |]


-- | Only call 'endMenu' if 'beginMenu' returns true!
--
-- Wraps @ImGui::EndMenu()@.
endMenu :: MonadIO m => m ()
endMenu = liftIO do
  [C.exp| void { EndMenu(); } |]


-- Return true when activated. Shortcuts are displayed for convenience but not
-- processed by ImGui at the moment
--
-- Wraps @ImGui::MenuItem()@
menuItem :: MonadIO m => String -> m Bool
menuItem label = liftIO do
  withCString label \labelPtr ->
    (1 ==) <$> [C.exp| bool { MenuItem($(char* labelPtr)) } |]


-- | A cardinal direction.
newtype ImGuiDir      = ImGuiDir CInt


pattern ImGuiDirLeft, ImGuiDirRight, ImGuiDirUp, ImGuiDirDown :: ImGuiDir
pattern ImGuiDirLeft  = ImGuiDir 0
pattern ImGuiDirRight = ImGuiDir 1
pattern ImGuiDirUp    = ImGuiDir 2
pattern ImGuiDirDown  = ImGuiDir 3


withCStringOrNull :: Maybe String -> (Ptr CChar -> IO a) -> IO a
withCStringOrNull Nothing k  = k nullPtr
withCStringOrNull (Just s) k = withCString s k
