{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module: DearImGui

Main ImGui module, exporting the functions to create a GUI.
-}

module DearImGui.Raw
  ( -- * Context Creation and Access
    Context(..)
  , createContext
  , destroyContext
  , getCurrentContext
  , setCurrentContext

    -- * Main
  , newFrame
  , endFrame
  , render
  , DrawData(..)
  , getDrawData
  , checkVersion

    -- * Demo, Debug, Information
  , showDemoWindow
  , showMetricsWindow
  , showDebugLogWindow
  , showIDStackToolWindow
  , showAboutWindow
  , showStyleSelector
  , showFontSelector
  , showUserGuide
  , getVersion
  , logButtons
  , logText

    -- * Styles
  , styleColorsDark
  , styleColorsLight
  , styleColorsClassic

    -- * Windows
  , begin
  , end

    -- ** Utilities

  , getWindowDrawList
  , getWindowPos
  , getWindowSize
  , getWindowWidth
  , getWindowHeight
  , isWindowAppearing
  , isWindowCollapsed
  , isWindowFocused

    -- ** Manipulation

  , setNextWindowPos
  , setNextWindowSize
  , setNextWindowFullscreen
  , setNextWindowContentSize
  , setNextWindowSizeConstraints
  , setNextWindowCollapsed
  , setNextWindowFocus
  , setNextWindowScroll
  , setNextWindowBgAlpha
  , getContentRegionAvail
  , getContentRegionMax
  , getWindowContentRegionMin
  , getWindowContentRegionMax
  , beginDisabled
  , endDisabled

  , setItemDefaultFocus
  , setKeyboardFocusHere

  , setNextItemAllowOverlap

    -- ** Child Windows
  , beginChild
  , beginChildContext
  , endChild

    -- * Parameter stacks
  , pushStyleColor
  , popStyleColor
  , pushStyleVar
  , popStyleVar
  , pushTabStop
  , popTabStop

    -- * Cursor/Layout
  , separator
  , sameLine
  , newLine
  , spacing
  , dummy
  , indent
  , unindent
  , setNextItemWidth
  , pushItemWidth
  , popItemWidth
  , calcItemWidth
  , pushTextWrapPos
  , popTextWrapPos
  , beginGroup
  , endGroup
  , getCursorPos
  , getCursorPosX
  , getCursorPosY
  , getCursorScreenPos
  , getCursorStartPos
  , setCursorPos
  , setCursorPosX
  , setCursorPosY
  , setCursorScreenPos
  , alignTextToFramePadding
  , getTextLineHeight
  , getTextLineHeightWithSpacing
  , getFrameHeight
  , getFrameHeightWithSpacing

    -- * Widgets
    -- ** Text
  , textUnformatted
  , textColored
  , textDisabled
  , textWrapped
  , labelText
  , bulletText
  , separatorText
  , valueBool
  , valueInt
  , valueUInt
  , valueFloat

    -- ** Main
  , button
  , smallButton
  , invisibleButton
  , arrowButton
  , image
  , imageButton
  , checkbox
  , checkboxFlags
  , checkboxFlagsU
  , radioButton
  , radioButtonI
  , progressBar
  , bullet

    -- ** Combo Box
  , beginCombo
  , endCombo
  , combo

    -- ** Drag Sliders
  , dragFloat
  , dragFloat2
  , dragFloat3
  , dragFloat4
  , dragFloatRange2
  , dragInt
  , dragInt2
  , dragInt3
  , dragInt4
  , dragIntRange2
  , dragScalar
  , dragScalarN

    -- ** Slider
  , sliderFloat
  , sliderFloat2
  , sliderFloat3
  , sliderFloat4
  , sliderAngle
  , sliderInt
  , sliderInt2
  , sliderInt3
  , sliderInt4
  , sliderScalar
  , sliderScalarN
  , vSliderFloat
  , vSliderInt
  , vSliderScalar

    -- ** Text Input
  , inputText
  , inputTextMultiline
  , inputTextWithHint
  , inputFloat
  , inputFloat2
  , inputFloat3
  , inputFloat4
  , inputInt
  , inputInt2
  , inputInt3
  , inputInt4
  , inputDouble
  , inputScalar
  , inputScalarN

    -- * Color Editor/Picker
  , colorEdit3
  , colorEdit4
  , colorPicker3
  , colorPicker4
  , colorButton
  , setColorEditOptions

    -- * Tables
  , beginTable
  , endTable
  , tableNextRow
  , tableNextColumn
  , tableSetColumnIndex

  , tableSetupColumn
  , tableSetupScrollFreeze
  , tableHeadersRow
  , tableHeader

  , tableGetSortSpecs
  , tableClearSortSpecsDirty

  , tableGetColumnCount
  , tableGetColumnIndex
  , tableGetRowIndex
  , tableGetColumnName
  , tableGetColumnFlags
  , tableSetColumnEnabled
  , tableSetBgColor

    -- * Trees
  , treeNode
  , treeNodeEx
  , treePush
  , treePop
  , getTreeNodeToLabelSpacing
  , collapsingHeader
  , setNextItemOpen

    -- ** Selectables
  , selectable

    -- ** List Boxes
  , listBox

    -- * Data Plotting
  , plotLines
  , plotHistogram

    -- ** Menus
  , beginMenuBar
  , endMenuBar
  , beginMainMenuBar
  , endMainMenuBar
  , beginMenu
  , endMenu
  , menuItem

    -- ** Tabs, tab bar
  , beginTabBar
  , endTabBar
  , beginTabItem
  , endTabItem
  , tabItemButton
  , setTabItemClosed

    -- * Tooltips
  , setItemTooltip
  , beginItemTooltip
  , beginTooltip
  , endTooltip

    -- * Popups/Modals
  , beginPopup
  , beginPopupModal
  , endPopup
  , openPopup
  , openPopupOnItemClick
  , closeCurrentPopup
  , beginPopupContextItem
  , beginPopupContextWindow
  , beginPopupContextVoid
  , isPopupOpen

    -- * ID stack/scopes
  , pushIDInt
  , pushIDPtr
  , pushIDStr
  , pushIDStrLen
  , popID

    -- * Item/Widgets Utilities
  , isItemHovered
  , isItemActive
  , isItemFocused
  , isItemClicked
  , isItemVisible
  , isItemEdited
  , isItemActivated
  , isItemDeactivated
  , isItemDeactivatedAfterEdit
  , isItemToggledOpen
  , isAnyItemHovered
  , isAnyItemActive
  , isAnyItemFocused
  , getItemID
  , getItemRectMin
  , getItemRectMax
  , getItemRectSize

  , wantCaptureMouse
  , getMousePos
  , getMousePosOnOpeningCurrentPopup
  , isMouseDragging
  , getMouseDragDelta
  , resetMouseDragDelta

  , wantCaptureKeyboard

    -- ** Inputs Utilities: Shortcut Testing & Routing @BETA@
  , ImGuiKeyChord
  , shortcut
  , setNextItemShortcut

    -- * Utilities

    -- ** Miscellaneous
  , getBackgroundDrawList
  , getForegroundDrawList
  , imCol32
  , framerate
  , getTime
  , getFrameCount

    -- ** Text utilities
  , calcTextSize

    -- * Types
  , module DearImGui.Enums
  , module DearImGui.Structs
  )
  where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign
import Foreign.C
import System.IO.Unsafe
  ( unsafePerformIO )

-- dear-imgui
import DearImGui.Raw.Context
  ( imguiContext )
import DearImGui.Enums
import DearImGui.Structs
import DearImGui.Raw.DrawList (DrawList(..))

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGuiContext*@.
newtype Context = Context (Ptr ImGuiContext)


-- | Wraps @ImGui::CreateContext()@.
createContext :: (MonadIO m) => m Context
createContext = liftIO do
  Context <$> [C.exp| ImGuiContext* { CreateContext() } |]


-- | Wraps @ImGui::DestroyContext()@.
destroyContext :: (MonadIO m) => Context -> m ()
destroyContext (Context contextPtr) = liftIO do
  [C.exp| void { DestroyContext($(ImGuiContext* contextPtr)); } |]

-- | Wraps @ImGui::GetCurrentContext()@.
getCurrentContext :: MonadIO m => m Context
getCurrentContext = liftIO do
  Context <$> [C.exp| ImGuiContext* { GetCurrentContext() } |]


-- | Wraps @ImGui::SetCurrentContext()@.
setCurrentContext :: MonadIO m => Context -> m ()
setCurrentContext (Context contextPtr) = liftIO do
  [C.exp| void { SetCurrentContext($(ImGuiContext* contextPtr)) } |]


-- | Start a new Dear ImGui frame, you can submit any command from this point
-- until 'render'/'endFrame'.
--
-- Wraps @ImGui::NewFrame()@.
newFrame :: (MonadIO m) => m ()
newFrame = liftIO do
  [C.exp| void { NewFrame(); } |]


-- | Ends the Dear ImGui frame. automatically called by 'render'. If you don't
-- need to render data (skipping rendering) you may call 'endFrame' without
-- 'render'... but you'll have wasted CPU already! If you don't need to render,
-- better to not create any windows and not call 'newFrame' at all!
endFrame :: (MonadIO m) => m ()
endFrame = liftIO do
  [C.exp| void { EndFrame(); } |]


-- | Ends the Dear ImGui frame, finalize the draw data. You can then get call
-- 'getDrawData'.
render :: (MonadIO m) => m ()
render = liftIO do
  [C.exp| void { Render(); } |]


-- | Wraps @ImDrawData*@.
newtype DrawData = DrawData (Ptr ())


-- | Valid after 'render' and until the next call to 'newFrame'. This is what
-- you have to render.
getDrawData :: (MonadIO m) => m DrawData
getDrawData = liftIO do
  DrawData <$> [C.exp| void* { GetDrawData() } |]


-- | Wraps @IMGUI_CHECKVERSION()@
checkVersion :: (MonadIO m) => m ()
checkVersion = liftIO do
  [C.exp| void { IMGUI_CHECKVERSION(); } |]


-- | Create demo window. Demonstrate most ImGui features. Call this to learn
-- about the library! Try to make it always available in your application!
showDemoWindow :: (MonadIO m) => m ()
showDemoWindow = liftIO do
  [C.exp| void { ShowDemoWindow(); } |]


-- | Create Metrics/Debugger window. Display Dear ImGui internals: windows, draw
-- commands, various internal state, etc.
showMetricsWindow :: (MonadIO m) => m ()
showMetricsWindow = liftIO do
  [C.exp| void { ShowMetricsWindow(); } |]

-- | Create Debug Log window. display a simplified log of important dear imgui events.
showDebugLogWindow :: (MonadIO m) => m ()
showDebugLogWindow = liftIO do
  [C.exp| void { ShowDebugLogWindow(); } |]

-- | Create Stack Tool window. hover items with mouse to query information about the source of their unique ID.
showIDStackToolWindow :: (MonadIO m) => m ()
showIDStackToolWindow = liftIO do
  [C.exp| void { ShowIDStackToolWindow(); } |]

-- | Create About window. display Dear ImGui version, credits and build/system
-- information.
showAboutWindow :: (MonadIO m) => m ()
showAboutWindow = liftIO do
  [C.exp| void { ShowAboutWindow(); } |]

{- TODO: requires ImGuiStyle.
-- | Add style editor block (not a window). you can pass in a reference "ImGuiStyle" structure to compare to, revert to and save to (else it uses the default style).
showStyleEditor :: (MonadIO m) => Ptr ImGuiStyle -> m ()
showStyleEditor = liftIO do
  [C.exp| void { ShowStyleEditor(); } |]
-}

-- | Add style selector block (not a window), essentially a combo listing the default styles.
showStyleSelector :: (MonadIO m) => CString -> m Bool
showStyleSelector labelPtr = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      ShowStyleSelector(
        $(char* labelPtr)
      )
    }
  |]

-- | Add font selector block (not a window), essentially a combo listing the loaded fonts.
showFontSelector :: (MonadIO m) => CString -> m ()
showFontSelector labelPtr = liftIO do
  [C.exp|
    void {
      ShowFontSelector(
        $(char* labelPtr)
      )
    }
  |]

-- | Add basic help/info block (not a window): how to manipulate ImGui as a
-- end-user (mouse/keyboard controls).
showUserGuide :: (MonadIO m) => m ()
showUserGuide = liftIO do
  [C.exp| void { ShowUserGuide() } |]


-- | Get the compiled version string e.g. "1.80 WIP" (essentially the value for
-- @IMGUI_VERSION@ from the compiled version of @imgui.cpp@).
getVersion :: (MonadIO m) => m CString
getVersion = liftIO do
  [C.exp| const char* { GetVersion() } |]

-- | Helper to display buttons for logging to tty/file/clipboard.
logButtons :: (MonadIO m) => m ()
logButtons = liftIO do
  [C.block| void { LogButtons(); } |]

-- | Pass text data straight to log (without being displayed).
logText :: (MonadIO m) => CString -> m ()
logText textPtr = liftIO do
  [C.block| void { LogText("%s", $(char* textPtr) ); }|]

-- | New, recommended style (default).
--
-- Wraps @ImGui::StyleColorsDark()@.
styleColorsDark :: (MonadIO m) => m ()
styleColorsDark = liftIO do
  [C.exp| void { StyleColorsDark(); } |]


-- | Best used with borders and a custom, thicker font.
--
-- Wraps @ImGui::StyleColorsLight()@.
styleColorsLight :: (MonadIO m) => m ()
styleColorsLight = liftIO do
  [C.exp| void { StyleColorsLight(); } |]


-- | Classic ImGui style.
--
-- Wraps @ImGui::StyleColorsClasic()@.
styleColorsClassic :: (MonadIO m) => m ()
styleColorsClassic = liftIO do
  [C.exp| void { StyleColorsClassic(); } |]


-- | Push window to the stack and start appending to it.
--
-- Returns 'False' to indicate the window is collapsed or fully clipped, so you
-- may early out and omit submitting anything to the window. Always call a
-- matching 'end' for each 'begin' call, regardless of its return value!
--
-- Wraps @ImGui::Begin()@.
--
-- Passing non-null @Ptr CBool@ shows a window-closing widget in the upper-right corner of the window,
-- wich clicking will set the boolean to false when clicked.
begin :: (MonadIO m) => CString -> Maybe (Ptr CBool) -> Maybe (ImGuiWindowFlags) -> m Bool
begin namePtr (Just openPtr) (Just flags) = liftIO do
  (0 /=) <$> [C.exp| bool { Begin($(char* namePtr), $(bool* openPtr), $(ImGuiWindowFlags flags)) } |]
begin namePtr (Just openPtr) Nothing = liftIO do
  (0 /=) <$> [C.exp| bool { Begin($(char* namePtr), $(bool* openPtr)) } |]
begin namePtr Nothing Nothing = liftIO do
  (0 /=) <$> [C.exp| bool { Begin($(char* namePtr)) } |]
begin _ Nothing _ = error "C++ default argument restriction."

-- | Pop window from the stack.
--
-- Wraps @ImGui::End()@.
end :: (MonadIO m) => m ()
end = liftIO do
  [C.exp| void { End(); } |]


-- | Begin a self-contained independent scrolling/clipping regions within a host window.
--
-- Child windows can embed their own child.
--
-- For each independent axis of @size@:
--   * ==0.0f: use remaining host window size
--   * >0.0f: fixed size
--   * <0.0f: use remaining window size minus abs(size)
--
-- Each axis can use a different mode, e.g. @ImVec2 0 400@.
--
-- @BeginChild()@ returns `False` to indicate the window is collapsed or fully clipped, so you may early out and omit submitting anything to the window.
--
-- Always call a matching `endChild` for each `beginChild` call, regardless of its return value.
--
-- Wraps @ImGui::BeginChild()@.
beginChild :: (MonadIO m) => CString -> Ptr ImVec2 -> CBool -> ImGuiWindowFlags -> m Bool
beginChild namePtr sizePtr border flags = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      BeginChild(
        $(char* namePtr),
        *$(ImVec2* sizePtr),
        $(bool border),
        $(ImGuiWindowFlags flags)
      )
    }
  |]

-- | Switch context to another child window by its ID
--
-- Wraps @ImGui::BeginChild()@.
beginChildContext :: (MonadIO m) => CString -> m Bool
beginChildContext namePtr = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      BeginChild(
        $(char* namePtr)
      )
    }
  |]

-- | Wraps @ImGui::EndChild()@.
endChild :: (MonadIO m) => m ()
endChild = liftIO do
  [C.exp| void { EndChild(); } |]

isWindowAppearing :: (MonadIO m) => m Bool
isWindowAppearing = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      IsWindowAppearing()
    }
  |]

isWindowCollapsed :: (MonadIO m) => m Bool
isWindowCollapsed = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      IsWindowCollapsed()
    }
  |]

isWindowFocused :: (MonadIO m) => ImGuiFocusedFlags -> m Bool
isWindowFocused flags = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      IsWindowFocused($(ImGuiFocusedFlags flags))
    }
  |]

-- | Separator, generally horizontal. inside a menu bar or in horizontal layout
-- mode, this becomes a vertical separator.
--
-- Wraps @ImGui::Separator()@
separator :: (MonadIO m) => m ()
separator = liftIO do
  [C.exp| void { Separator(); } |]


-- | Call between widgets or groups to layout them horizontally.
--
-- Wraps @ImGui::SameLine@.
sameLine :: (MonadIO m) => m ()
sameLine = liftIO do
  [C.exp| void { SameLine(); } |]

-- | Raw text without formatting.
--
-- Roughly equivalent to Text("%s", text) but:
--   A) doesn't require null terminated string if 'text_end' is specified,
--   B) it's faster, no memory copy is done, no buffer size limits, recommended for long chunks of text.
--
-- Wraps @ImGui::TextUnformatted()@.
textUnformatted :: (MonadIO m) => CString -> Maybe CString -> m ()
textUnformatted textPtr (Just textEndPtr) = liftIO do
  [C.exp| void { TextUnformatted($(char* textPtr), $(char* textEndPtr)) } |]
textUnformatted textPtr Nothing = liftIO do
  [C.exp| void { TextUnformatted($(char* textPtr)) } |]

-- | Shortcut for @PushStyleColor(ImGuiCol_Text, col); Text(fmt, ...); PopStyleColor();@.
--
-- XXX: Unlike the original, does not do string formatting.
--
-- Wraps @ImGui::TextColored()@.
textColored :: (MonadIO m) => Ptr ImVec4 -> CString -> m ()
textColored colorPtr textPtr = liftIO do
  [C.exp| void { TextColored(*$(ImVec4 *colorPtr), "%s", $(char* textPtr)) } |]

-- | Shortcut for @PushStyleColor(ImGuiCol_Text, style.Colors[ImGuiCol_TextDisabled]); Text(fmt, ...); PopStyleColor();@.
--
-- XXX: Unlike the original, does not do string formatting.
--
-- Wraps @ImGui::TextWrapped()@.
textDisabled :: (MonadIO m) => CString -> m ()
textDisabled textPtr = liftIO do
  [C.exp| void { TextDisabled("%s", $(char* textPtr)) } |]

-- | Shortcut for @PushTextWrapPos(0.0f); Text(fmt, ...); PopTextWrapPos();@.
--
-- Note that this won't work on an auto-resizing window if there's no other widgets to extend the window width,
-- you may need to set a size using 'setNextWindowSize'.
--
-- XXX: Unlike the original, does not do string formatting.
--
-- Wraps @ImGui::TextWrapped()@.
textWrapped :: (MonadIO m) => CString -> m ()
textWrapped textPtr = liftIO do
  [C.exp| void { TextWrapped("%s", $(char* textPtr)) } |]

-- | Label+text combo aligned to other label+value widgets.
--
-- XXX: Unlike the original, does not do string formatting.
--
-- Wraps @ImGui::LabelText()@.
labelText :: (MonadIO m) => CString -> CString -> m ()
labelText labelPtr textPtr = liftIO do
  [C.exp| void { LabelText($(char* labelPtr), "%s", $(char* textPtr)) } |]

-- | Text with a little bullet aligned to the typical tree node.
--
-- XXX: Unlike the original, does not do string formatting.
--
-- Wraps @ImGui::BulletText()@.
bulletText :: (MonadIO m) => CString -> m ()
bulletText textPtr = liftIO do
  [C.exp| void { BulletText("%s", $(char* textPtr)) } |]

-- | Text with an horizontal line.
separatorText :: (MonadIO m) => CString -> m ()
separatorText textPtr = liftIO do
  [C.block| void { SeparatorText($(char* textPtr)); } |]

valueBool :: (MonadIO m) => CString -> CBool -> m ()
valueBool labelPtr b = liftIO do
  [C.block| void { Value($(char* labelPtr), $(bool b)); } |]

valueInt :: (MonadIO m) => CString -> CInt -> m ()
valueInt labelPtr v = liftIO do
  [C.block| void { Value($(char* labelPtr), $(int v)); } |]

valueUInt :: (MonadIO m) => CString -> CUInt -> m ()
valueUInt labelPtr v = liftIO do
  [C.block| void { Value($(char* labelPtr), $(unsigned int v)); } |]

valueFloat :: (MonadIO m) => CString -> CFloat -> CString -> m ()
valueFloat labelPtr v formatPtr = liftIO do
  [C.block| void { Value($(char* labelPtr), $(float v), $(char* formatPtr)); } |]

-- | A button. Returns 'True' when clicked.
--
-- Wraps @ImGui::Button()@.
button :: (MonadIO m) => CString -> m Bool
button labelPtr = liftIO do
  (0 /=) <$> [C.exp| bool { Button($(char* labelPtr)) } |]


-- | Button with @FramePadding=(0,0)@ to easily embed within text.
--
-- Wraps @ImGui::SmallButton()@.
smallButton :: (MonadIO m) => CString -> m Bool
smallButton labelPtr = liftIO do
  (0 /=) <$> [C.exp| bool { SmallButton($(char* labelPtr)) } |]


-- | Flexible button behavior without the visuals.
--
-- Frequently useful to build custom behaviors using the public api
-- (along with IsItemActive, IsItemHovered, etc).
--
-- Wraps @ImGui::InvisibleButton()@.
invisibleButton :: (MonadIO m) => CString -> Ptr ImVec2 -> ImGuiButtonFlags -> m Bool
invisibleButton labelPtr size flags = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      InvisibleButton(
        $(char* labelPtr),
        *$(ImVec2* size),
        $(ImGuiButtonFlags flags)
      )
    }
  |]

-- | Square button with an arrow shape.
--
-- Wraps @ImGui::ArrowButton()@.
arrowButton :: (MonadIO m) => CString -> ImGuiDir -> m Bool
arrowButton strIdPtr dir = liftIO do
  (0 /=) <$> [C.exp| bool { ArrowButton($(char* strIdPtr), $(ImGuiDir dir)) } |]


-- | Image Area to draw a texture.
--
-- For OpenGL: The @userTextureIDPtr@ points to the texture memory (eg. @0x0000000000000001@)
--
-- See @examples/sdl/Image.hs@ for the whole process.
--
-- Wraps @ImGui::Image()@.
image :: (MonadIO m) => Ptr () -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec4 -> Ptr ImVec4 -> m ()
image userTextureIDPtr sizePtr uv0Ptr uv1Ptr tintColPtr borderColPtr = liftIO do
  [C.exp|
    void {
      Image(
        $(void* userTextureIDPtr),
        *$(ImVec2* sizePtr),
        *$(ImVec2* uv0Ptr),
        *$(ImVec2* uv1Ptr),
        *$(ImVec4* tintColPtr),
        *$(ImVec4* borderColPtr)
      )
    }
  |]

-- | Clickable Image Area.
--
-- Negative @frame_padding@ uses default frame padding settings. Set to 0 for no padding.
--
-- Wraps @ImGui::ImageButton()@.
imageButton :: (MonadIO m) => Ptr () -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> CInt -> Ptr ImVec4 -> Ptr ImVec4 -> m Bool
imageButton userTextureIDPtr sizePtr uv0Ptr uv1Ptr framePadding bgColPtr tintColPtr = liftIO do
  (0 /=) <$> [C.exp|
    bool {
      ImageButton(
        $(void* userTextureIDPtr),
        *$(ImVec2* sizePtr),
        *$(ImVec2* uv0Ptr),
        *$(ImVec2* uv1Ptr),
        $(int framePadding),
        *$(ImVec4* bgColPtr),
        *$(ImVec4* tintColPtr)
      )
    }
  |]


-- | Wraps @ImGui::Checkbox()@.
checkbox :: (MonadIO m) => CString -> Ptr CBool -> m Bool
checkbox labelPtr boolPtr = liftIO do
  (0 /=) <$> [C.exp| bool { Checkbox($(char* labelPtr), $(bool* boolPtr)) } |]

-- | A checkbox toggling a bit mask (signed).
checkboxFlags :: (MonadIO m) => CString -> Ptr CInt -> CInt -> m Bool
checkboxFlags labelPtr flagsPtr flagsValue = liftIO do
  (0 /=) <$> [C.exp| bool { CheckboxFlags($(char* labelPtr), $(int* flagsPtr), $(int flagsValue)) } |]

-- | A checkbox toggling a bit mask (unsigned).
checkboxFlagsU :: (MonadIO m) => CString -> Ptr CUInt -> CUInt -> m Bool
checkboxFlagsU labelPtr flagsPtr flagsValue = liftIO do
  (0 /=) <$> [C.exp| bool { CheckboxFlags($(char* labelPtr), $(unsigned int* flagsPtr), $(unsigned int flagsValue)) } |]

radioButton :: (MonadIO m) => CString -> CBool -> m Bool
radioButton labelPtr active = liftIO do
  (0 /=) <$> [C.exp| bool { RadioButton($(char* labelPtr), $(bool active)) } |]

-- | A shortcut for "radioButton" when the value is an integer.
radioButtonI :: (MonadIO m) => CString -> Ptr CInt -> CInt -> m Bool
radioButtonI labelPtr v vButton  = liftIO do
  (0 /=) <$> [C.exp| bool { RadioButton($(char* labelPtr), $(int* v), $(int vButton)) } |]

-- TODO: publish ImVec2(-FLT_MIN, 0)
-- | Wraps @ImGui::ProgressBar()@.
progressBar :: (MonadIO m) => CFloat -> CString -> m ()
progressBar progress overlayPtr = liftIO do
    [C.exp| void { ProgressBar($(float progress), ImVec2(-FLT_MIN, 0), $(char* overlayPtr)) } |]


-- | Draw a small circle + keep the cursor on the same line. Advance cursor x
-- position by 'getTreeNodeToLabelSpacing', same distance that 'treeNode' uses.
bullet :: (MonadIO m) => m ()
bullet = liftIO do
  [C.exp| void { Bullet() } |]


-- | Begin creating a combo box with a given label and preview value.
--
-- Returns 'True' if the combo box is open. In this state, you should populate
-- the contents of the combo box - for example, by calling 'selectable'.
--
-- Wraps @ImGui::BeginCombo()@.
beginCombo :: (MonadIO m) => CString -> CString -> m Bool
beginCombo labelPtr previewValuePtr = liftIO do
  (0 /=) <$> [C.exp| bool { BeginCombo($(char* labelPtr), $(char* previewValuePtr)) } |]


-- | Only call 'endCombo' if 'beginCombo' returns 'True'!
--
-- Wraps @ImGui::EndCombo()@.
endCombo :: (MonadIO m) => m ()
endCombo = liftIO do
  [C.exp| void { EndCombo() } |]


-- | Wraps @ImGui::Combo()@.
combo :: (MonadIO m) => CString -> Ptr CInt -> Ptr CString -> CInt -> m Bool
combo labelPtr iPtr itemsPtr itemsLen = liftIO do
  (0 /=) <$> [C.exp| bool { Combo($(char* labelPtr), $(int* iPtr), $(char** itemsPtr), $(int itemsLen)) }|]


-- | Wraps @ImGui::DragFloat()@
dragFloat :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CFloat -> CString -> m Bool
dragFloat descPtr floatPtr speed minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { DragFloat( $(char* descPtr), $(float* floatPtr), $(float speed), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]


-- | Wraps @ImGui::DragFloat2()@
dragFloat2 :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CFloat -> CString -> m Bool
dragFloat2 descPtr floatPtr speed minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { DragFloat2( $(char* descPtr), $(float* floatPtr), $(float speed), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]


-- | Wraps @ImGui::DragFloat3()@
dragFloat3 :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CFloat -> CString -> m Bool
dragFloat3 descPtr floatPtr speed minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { DragFloat3( $(char* descPtr), $(float* floatPtr), $(float speed), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]


-- | Wraps @ImGui::DragFloat4()@
dragFloat4 :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CFloat -> CString -> m Bool
dragFloat4 descPtr floatPtr speed minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { DragFloat4( $(char* descPtr), $(float* floatPtr), $(float speed), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]


-- | Wraps @ImGui::DragFloatRange2()@
dragFloatRange2 :: (MonadIO m) => CString -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> CFloat -> CString -> CString -> ImGuiSliderFlags -> m Bool
dragFloatRange2 labelPtr vCurrentMin vCurrentMax vSpeed vMin vMax formatMin formatMax flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragFloatRange2(
      $(char* labelPtr),
      $(float* vCurrentMin),
      $(float* vCurrentMax),
      $(float vSpeed),
      $(float vMin),
      $(float vMax),
      $(char* formatMin),
      $(char* formatMax),
      $(ImGuiSliderFlags flags)
    )
  } |]


-- | Wraps @ImGui::DragInt()@
dragInt :: (MonadIO m) => CString -> Ptr CInt -> CFloat -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
dragInt labelPtr vPtr vSpeed vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragInt(
      $(char* labelPtr),
      $(int* vPtr),
      $(float vSpeed),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::DragInt2()@
dragInt2 :: (MonadIO m) => CString -> Ptr CInt -> CFloat -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
dragInt2 labelPtr vPtr vSpeed vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragInt2(
      $(char* labelPtr),
      $(int vPtr[2]),
      $(float vSpeed),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::DragInt3()@
dragInt3 :: (MonadIO m) => CString -> Ptr CInt -> CFloat -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
dragInt3 labelPtr vPtr vSpeed vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragInt3(
      $(char* labelPtr),
      $(int vPtr[3]),
      $(float vSpeed),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::DragInt4()@
dragInt4 :: (MonadIO m) => CString -> Ptr CInt -> CFloat -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
dragInt4 labelPtr vPtr vSpeed vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragInt4(
      $(char* labelPtr),
      $(int vPtr[4]),
      $(float vSpeed),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::DragFloatRange2()@
dragIntRange2 :: (MonadIO m) => CString -> Ptr CInt -> Ptr CInt -> CFloat -> CInt -> CInt -> CString -> CString -> ImGuiSliderFlags -> m Bool
dragIntRange2 labelPtr vCurrentMin vCurrentMax vSpeed vMin vMax formatMin formatMax flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragIntRange2(
      $(char* labelPtr),
      $(int* vCurrentMin),
      $(int* vCurrentMax),
      $(float vSpeed),
      $(int vMin),
      $(int vMax),
      $(char* formatMin),
      $(char* formatMax),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::DragScalar()@
dragScalar :: (MonadIO m) => CString -> ImGuiDataType -> Ptr a -> CFloat -> Ptr a -> Ptr a -> CString -> ImGuiSliderFlags -> m Bool
dragScalar labelPtr dataType dataPtr vSpeed minPtr maxPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragScalar(
      $(char* labelPtr),
      $(ImGuiDataType dataType),
      $(void* dataPtr_),
      $(float vSpeed),
      $(void* minPtr_),
      $(void* maxPtr_),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]
  where
    dataPtr_ = castPtr dataPtr
    minPtr_ = castPtr minPtr
    maxPtr_ = castPtr maxPtr

-- | Wraps @ImGui::DragScalarN()@
dragScalarN :: (MonadIO m) => CString -> ImGuiDataType -> Ptr a -> CInt -> CFloat -> Ptr a -> Ptr a -> CString -> ImGuiSliderFlags -> m Bool
dragScalarN labelPtr dataType dataPtr components vSpeed minPtr maxPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    DragScalarN(
      $(char* labelPtr),
      $(ImGuiDataType dataType),
      $(void* dataPtr_),
      $(int components),
      $(float vSpeed),
      $(void* minPtr_),
      $(void* maxPtr_),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]
  where
    dataPtr_ = castPtr dataPtr
    minPtr_ = castPtr minPtr
    maxPtr_ = castPtr maxPtr

-- | Wraps @ImGui::SliderFloat()@
sliderFloat :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CString -> m Bool
sliderFloat descPtr floatPtr minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { SliderFloat( $(char* descPtr), $(float* floatPtr), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]


-- | Wraps @ImGui::SliderFloat2()@
sliderFloat2 :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CString -> m Bool
sliderFloat2 descPtr floatPtr minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { SliderFloat2( $(char* descPtr), $(float* floatPtr), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]


-- | Wraps @ImGui::SliderFloat3()@
sliderFloat3 :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CString -> m Bool
sliderFloat3 descPtr floatPtr minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { SliderFloat3( $(char* descPtr), $(float* floatPtr), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]


-- | Wraps @ImGui::SliderFloat4()@
sliderFloat4 :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CString -> m Bool
sliderFloat4 descPtr floatPtr minValue maxValue formatPtr = liftIO do
  (0 /=) <$> [C.exp| bool { SliderFloat4( $(char* descPtr), $(float* floatPtr), $(float minValue), $(float maxValue), $(char* formatPtr)) } |]

-- | Wraps @ImGui::SliderAngle()@
sliderAngle :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CString -> ImGuiSliderFlags -> m Bool
sliderAngle descPtr valueRadPtr degreesMin degreesMax format flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    SliderAngle(
      $(char* descPtr),
      $(float* valueRadPtr),
      $(float degreesMin),
      $(float degreesMax),
      $(char* format),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::SliderInt()@
sliderInt :: (MonadIO m) => CString -> Ptr CInt -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
sliderInt labelPtr vPtr vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    SliderInt(
      $(char* labelPtr),
      $(int* vPtr),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::SliderInt2()@
sliderInt2 :: (MonadIO m) => CString -> Ptr CInt -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
sliderInt2 labelPtr vPtr vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    SliderInt2(
      $(char* labelPtr),
      $(int vPtr[2]),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::SliderInt3()@
sliderInt3 :: (MonadIO m) => CString -> Ptr CInt -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
sliderInt3 labelPtr vPtr vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    SliderInt3(
      $(char* labelPtr),
      $(int vPtr[3]),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::SliderInt4()@
sliderInt4 :: (MonadIO m) => CString -> Ptr CInt -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
sliderInt4 labelPtr vPtr vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    SliderInt4(
      $(char* labelPtr),
      $(int vPtr[4]),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::SliderScalar()@
sliderScalar :: (MonadIO m) => CString -> ImGuiDataType -> Ptr a -> Ptr a -> Ptr a -> CString -> ImGuiSliderFlags -> m Bool
sliderScalar labelPtr dataType dataPtr minPtr maxPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    SliderScalar(
      $(char* labelPtr),
      $(ImGuiDataType dataType),
      $(void* dataPtr_),
      $(void* minPtr_),
      $(void* maxPtr_),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]
  where
    dataPtr_ = castPtr dataPtr
    minPtr_ = castPtr minPtr
    maxPtr_ = castPtr maxPtr

-- | Wraps @ImGui::SliderScalarN()@
sliderScalarN :: (MonadIO m) => CString -> ImGuiDataType -> Ptr a -> CInt -> Ptr a -> Ptr a -> CString -> ImGuiSliderFlags -> m Bool
sliderScalarN labelPtr dataType dataPtr components minPtr maxPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    SliderScalarN(
      $(char* labelPtr),
      $(ImGuiDataType dataType),
      $(void* dataPtr_),
      $(int components),
      $(void* minPtr_),
      $(void* maxPtr_),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]
  where
    dataPtr_ = castPtr dataPtr
    minPtr_ = castPtr minPtr
    maxPtr_ = castPtr maxPtr

-- | Wraps @ImGui::VSliderFloat()@
vSliderFloat :: (MonadIO m) => CString -> Ptr ImVec2 -> Ptr CFloat -> CFloat -> CFloat -> CString -> ImGuiSliderFlags -> m Bool
vSliderFloat labelPtr sizePtr vPtr vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    VSliderFloat(
      $(char* labelPtr),
      *$(ImVec2* sizePtr),
      $(float* vPtr),
      $(float vMin),
      $(float vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::VSliderFloat()@
vSliderInt :: (MonadIO m) => CString -> Ptr ImVec2 -> Ptr CInt -> CInt -> CInt -> CString -> ImGuiSliderFlags -> m Bool
vSliderInt labelPtr sizePtr vPtr vMin vMax formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    VSliderInt(
      $(char* labelPtr),
      *$(ImVec2* sizePtr),
      $(int* vPtr),
      $(int vMin),
      $(int vMax),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]

-- | Wraps @ImGui::VSliderScalar()@
vSliderScalar :: (MonadIO m) => CString -> Ptr ImVec2 -> ImGuiDataType -> Ptr a -> Ptr a -> Ptr a -> CString -> ImGuiSliderFlags -> m Bool
vSliderScalar labelPtr sizePtr dataType dataPtr minPtr maxPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    VSliderScalar(
      $(char* labelPtr),
      *$(ImVec2* sizePtr),
      $(ImGuiDataType dataType),
      $(void* dataPtr_),
      $(void* minPtr_),
      $(void* maxPtr_),
      $(char* formatPtr),
      $(ImGuiSliderFlags flags)
    )
  } |]
  where
    dataPtr_ = castPtr dataPtr
    minPtr_ = castPtr minPtr
    maxPtr_ = castPtr maxPtr


-- | Wraps @ImGui::InputText()@.
inputText :: (MonadIO m) => CString -> CStringLen -> ImGuiInputTextFlags -> m Bool
inputText labelPtr (bufPtr, fromIntegral -> bufSize) flags = liftIO do
  (0 /= ) <$> [C.exp|
    bool {
      InputText(
        $(char* labelPtr),
        $(char* bufPtr),
        $(int bufSize),
        $(ImGuiInputTextFlags flags)
      )
    }
  |]

-- | Wraps @ImGui::InputTextMultiline()@.
inputTextMultiline :: (MonadIO m) => CString -> CStringLen -> Ptr ImVec2 -> ImGuiInputTextFlags -> m Bool
inputTextMultiline labelPtr (bufPtr, fromIntegral -> bufSize) sizePtr flags = liftIO do
  (0 /= ) <$> [C.exp|
    bool {
      InputTextMultiline(
        $(char* labelPtr),
        $(char* bufPtr),
        $(size_t bufSize),
        *$(ImVec2* sizePtr),
        $(ImGuiInputTextFlags flags)
      )
    }
  |]

-- | Wraps @ImGui::InputTextWithHint()@.
inputTextWithHint :: (MonadIO m) => CString -> CString -> CStringLen -> ImGuiInputTextFlags -> m Bool
inputTextWithHint labelPtr hintPtr (bufPtr, fromIntegral -> bufSize) flags = liftIO do
  (0 /= ) <$> [C.exp|
    bool {
      InputTextWithHint(
        $(char* labelPtr),
        $(char* hintPtr),
        $(char* bufPtr),
        $(int bufSize),
        $(ImGuiInputTextFlags flags)
      )
    }
  |]

inputFloat :: (MonadIO m) => CString -> Ptr CFloat -> CFloat -> CFloat -> CString -> ImGuiInputTextFlags -> m Bool
inputFloat descPtr floatPtr step stepFast formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputFloat( $(char* descPtr), $(float* floatPtr), $(float step), $(float stepFast), $(char* formatPtr), $(ImGuiInputTextFlags flags)) } |]

inputFloat2 :: (MonadIO m) => CString -> Ptr CFloat -> CString -> ImGuiInputTextFlags -> m Bool
inputFloat2 descPtr floatPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputFloat2( $(char* descPtr), $(float* floatPtr), $(char* formatPtr), $(ImGuiInputTextFlags flags)) } |]

inputFloat3 :: (MonadIO m) => CString -> Ptr CFloat -> CString -> ImGuiInputTextFlags -> m Bool
inputFloat3 descPtr floatPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputFloat3( $(char* descPtr), $(float* floatPtr), $(char* formatPtr), $(ImGuiInputTextFlags flags)) } |]

inputFloat4 :: (MonadIO m) => CString -> Ptr CFloat -> CString -> ImGuiInputTextFlags -> m Bool
inputFloat4 descPtr floatPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputFloat4( $(char* descPtr), $(float* floatPtr), $(char* formatPtr), $(ImGuiInputTextFlags flags)) } |]

inputInt :: (MonadIO m) => CString -> Ptr CInt -> CInt -> CInt -> ImGuiInputTextFlags -> m Bool
inputInt descPtr intPtr step stepFast flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputInt( $(char* descPtr), $(int* intPtr), $(int step), $(int stepFast), $(ImGuiInputTextFlags flags)) } |]

inputInt2 :: (MonadIO m) => CString -> Ptr CInt -> ImGuiInputTextFlags -> m Bool
inputInt2 descPtr intPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputInt2( $(char* descPtr), $(int* intPtr), $(ImGuiInputTextFlags flags)) } |]

inputInt3 :: (MonadIO m) => CString -> Ptr CInt -> ImGuiInputTextFlags -> m Bool
inputInt3 descPtr intPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputInt3( $(char* descPtr), $(int* intPtr), $(ImGuiInputTextFlags flags)) } |]

inputInt4 :: (MonadIO m) => CString -> Ptr CInt -> ImGuiInputTextFlags -> m Bool
inputInt4 descPtr intPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputInt4( $(char* descPtr), $(int* intPtr), $(ImGuiInputTextFlags flags)) } |]

inputDouble :: (MonadIO m) => CString -> Ptr CDouble -> CDouble -> CDouble -> CString -> ImGuiInputTextFlags -> m Bool
inputDouble descPtr doublePtr step stepFast formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { InputDouble( $(char* descPtr), $(double* doublePtr), $(double step), $(double stepFast), $(char* formatPtr), $(ImGuiInputTextFlags flags)) } |]

inputScalar :: (MonadIO m) => CString -> ImGuiDataType -> Ptr a -> Ptr a -> Ptr a -> CString -> ImGuiInputTextFlags -> m Bool
inputScalar labelPtr dataType dataPtr stepPtr stepMaxPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    InputScalar(
      $(char* labelPtr),
      $(ImGuiDataType dataType),
      $(void* dataPtr_),
      $(void* stepPtr_),
      $(void* stepMaxPtr_),
      $(char* formatPtr),
      $(ImGuiInputTextFlags flags)
    )
  } |]
  where
    dataPtr_ = castPtr dataPtr
    stepPtr_ = castPtr stepPtr
    stepMaxPtr_ = castPtr stepMaxPtr

inputScalarN :: (MonadIO m) => CString -> ImGuiDataType -> Ptr a -> CInt -> Ptr a -> Ptr a -> CString -> ImGuiInputTextFlags -> m Bool
inputScalarN labelPtr dataType dataPtr components stepPtr stepMaxPtr formatPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool {
    InputScalarN(
      $(char* labelPtr),
      $(ImGuiDataType dataType),
      $(void* dataPtr_),
      $(int components),
      $(void* stepPtr_),
      $(void* stepMaxPtr_),
      $(char* formatPtr),
      $(ImGuiInputTextFlags flags)
    )
  } |]
  where
    dataPtr_ = castPtr dataPtr
    stepPtr_ = castPtr stepPtr
    stepMaxPtr_ = castPtr stepMaxPtr

-- | Wraps @ImGui::ColorPicker3()@.
colorEdit3 :: (MonadIO m) => CString -> Ptr CFloat -> ImGuiColorEditFlags -> m Bool
colorEdit3 descPtr refPtr flags = liftIO do
  (0 /= ) <$> [C.exp| bool { ColorEdit3( $(char* descPtr), $(float* refPtr), $(ImGuiColorEditFlags flags) ) } |]

colorEdit4 :: (MonadIO m) => CString -> Ptr CFloat -> ImGuiColorEditFlags -> m Bool
colorEdit4 descPtr refPtr flags = liftIO do
  (0 /= ) <$> [C.exp| bool { ColorEdit4( $(char* descPtr), $(float* refPtr), $(ImGuiColorEditFlags flags) ) } |]

-- | Wraps @ImGui::ColorPicker3()@.
colorPicker3 :: (MonadIO m) => CString -> Ptr CFloat -> ImGuiColorEditFlags -> m Bool
colorPicker3 descPtr refPtr flags = liftIO do
  (0 /= ) <$> [C.exp| bool { ColorPicker3( $(char* descPtr), $(float* refPtr), $(ImGuiColorEditFlags flags) ) } |]

colorPicker4 :: (MonadIO m) => CString -> Ptr CFloat -> ImGuiColorEditFlags -> Ptr CFloat -> m Bool
colorPicker4 descPtr refPtr flags refColPtr = liftIO do
  (0 /= ) <$> [C.exp| bool { ColorPicker4( $(char* descPtr), $(float* refPtr), $(ImGuiColorEditFlags flags), $(float* refColPtr) ) } |]

-- | Initialize current options (generally on application startup) if you want to select a default format, picker type, etc.
--
-- User will be able to change many settings, unless you pass the `ImGuiColorEditFlags_NoOptions` flag to your calls.
setColorEditOptions :: (MonadIO m) => ImGuiColorEditFlags -> m ()
setColorEditOptions flags = liftIO do
  [C.block| void { SetColorEditOptions( $(ImGuiColorEditFlags flags) ); } |]

-- | Display a color square/button, hover for details, return true when pressed.
--
-- Wraps @ImGui::ColorButton()@.
colorButton :: (MonadIO m) => CString -> Ptr ImVec4 -> m Bool
colorButton descPtr refPtr = liftIO do
  (0 /=) <$> [C.exp| bool { ColorButton( $(char* descPtr), *$(ImVec4* refPtr) ) } |]


-- | Wraps @ImGui::BeginTable()@.
beginTable :: MonadIO m => CString -> CInt -> ImGuiTableFlags -> Ptr ImVec2 -> CFloat -> m Bool
beginTable labelPtr column flags outerSizePtr innerWidth = liftIO do
  (0 /=) <$> [C.exp| bool { BeginTable($(char* labelPtr), $(int column), $(ImGuiTableFlags flags), *$(ImVec2* outerSizePtr), $(float innerWidth)) } |]

-- | Only call 'endTable' if 'beginTable' returns true!
--
-- Wraps @ImGui::EndTable()@.
endTable :: MonadIO m => m ()
endTable = liftIO do
  [C.exp| void { EndTable() } |]

-- | Wraps @ImGui::TableNextRow()@.
--   append into the first cell of a new row.
tableNextRow :: MonadIO m => ImGuiTableRowFlags -> CFloat -> m ()
tableNextRow flags minRowHeight = liftIO do
  [C.exp| void { TableNextRow($(ImGuiTableRowFlags flags), $(float minRowHeight)) } |]

-- | Wraps @ImGui::TableNextColumn()@.
--   append into the next column (or first column of next row if currently in
--   last column). Return true when column is visible.
tableNextColumn :: MonadIO m => m Bool
tableNextColumn = liftIO do
  (0 /=) <$> [C.exp| bool { TableNextColumn() } |]

-- | Wraps @ImGui::TableSetColumnIndex()@.
--   append into the specified column. Return true when column is visible.
tableSetColumnIndex :: MonadIO m => CInt -> m Bool
tableSetColumnIndex column= liftIO do
  (0 /=) <$> [C.exp| bool { TableSetColumnIndex($(int column)) } |]

-- | Wraps @ImGui::TableSetupColumn()@.
tableSetupColumn :: MonadIO m => CString -> ImGuiTableColumnFlags -> CFloat -> ImGuiID-> m ()
tableSetupColumn labelPtr flags initWidthOrWeight userId = liftIO do
  [C.exp| void { TableSetupColumn($(char* labelPtr), $(ImGuiTableColumnFlags flags), $(float initWidthOrWeight), $(ImGuiID userId)) } |]

-- | Wraps @ImGui::TableSetupScrollFreeze()@.
tableSetupScrollFreeze :: MonadIO m => CInt -> CInt -> m ()
tableSetupScrollFreeze cols rows = liftIO do
  [C.exp| void { TableSetupScrollFreeze($(int cols), $(int rows)) } |]

-- | Wraps @ImGui::TableHeadersRow()@.
--   submit all headers cells based on data provided to 'tableSetupColumn'
--   + submit context menu
tableHeadersRow :: MonadIO m => m ()
tableHeadersRow = liftIO do
  [C.exp| void { TableHeadersRow() } |]

-- | Wraps @ImGui::TableHeader()@.
--   submit one header cell manually (rarely used)
tableHeader :: MonadIO m => CString -> m ()
tableHeader labelPtr = liftIO do
  [C.exp| void { TableHeader($(char* labelPtr)) } |]

-- | Wraps @ImGui::TableGetSortSpecs()@.
--   Low-level-Function. Better use the wrapper that outomatically conform
--   to the things described below
--
--   Tables: Sorting
--   - Call TableGetSortSpecs() to retrieve latest sort specs for the table.
--     NULL when not sorting.
--   - When 'SpecsDirty == true' you should sort your data. It will be true when
--     sorting specs have changed since last call, or the first time. Make sure
--     to set 'SpecsDirty = false' after sorting, else you may wastefully sort
--     your data every frame!
--   - Lifetime: don't hold on this pointer over multiple frames or past any
--     subsequent call to BeginTable().
tableGetSortSpecs :: MonadIO m => m (Maybe (Ptr ImGuiTableSortSpecs))
tableGetSortSpecs = liftIO do
  ptr <- [C.exp| ImGuiTableSortSpecs* { TableGetSortSpecs() } |]
  if ptr == nullPtr then
    return Nothing
  else
    return $ Just ptr

tableClearSortSpecsDirty :: MonadIO m => Ptr ImGuiTableSortSpecs -> m ()
tableClearSortSpecsDirty specsPtr = liftIO do
  [C.block| void {
    $(ImGuiTableSortSpecs* specsPtr)->SpecsDirty = false;
  } |]

-- | Wraps @ImGui::TableGetColumnCount()@.
tableGetColumnCount :: MonadIO m => m CInt
tableGetColumnCount = liftIO do
  [C.exp| int { TableGetColumnCount() } |]

-- | Wraps @ImGui::TableGetColumnIndex()@.
tableGetColumnIndex :: MonadIO m => m CInt
tableGetColumnIndex = liftIO do
  [C.exp| int { TableGetColumnIndex() } |]

-- | Wraps @ImGui::TableGetRowIndex()@.
tableGetRowIndex :: MonadIO m => m CInt
tableGetRowIndex = liftIO do
  [C.exp| int { TableGetRowIndex() } |]

-- | Wraps @ImGui::TableGetColumnName
--   'Nothing' returns the current column name
tableGetColumnName :: MonadIO m => Maybe CInt -> m CString
tableGetColumnName Nothing = tableGetColumnName (Just (-1))
tableGetColumnName (Just column_n) = liftIO do
  [C.exp| const char* { TableGetColumnName($(int column_n)) } |]

-- | Wraps @ImGui::TableGetRowIndex()@.
--   'Nothing' returns the current column flags
tableGetColumnFlags :: MonadIO m => Maybe CInt -> m ImGuiTableColumnFlags
tableGetColumnFlags Nothing = tableGetColumnFlags (Just (-1))
tableGetColumnFlags (Just column_n) = liftIO do
  [C.exp| ImGuiTableColumnFlags { TableGetColumnFlags($(int column_n)) } |]

-- | Wraps @ImGui::TableSetColumnEnabled()@.
tableSetColumnEnabled :: MonadIO m => CInt -> CBool -> m ()
tableSetColumnEnabled column_n v = liftIO do
  [C.exp| void { TableSetColumnEnabled($(int column_n), $(bool v)) } |]

-- | Wraps @ImGui::TableSetBgColor()@.
--   'Nothing' sets the current row/column color
tableSetBgColor :: MonadIO m => ImGuiTableBgTarget -> ImU32 -> Maybe CInt -> m ()
tableSetBgColor target color Nothing = tableSetBgColor target color (Just (-1))
tableSetBgColor target color (Just column_n) = liftIO do
  [C.exp| void { TableSetBgColor($(ImGuiTableBgTarget target), $(ImU32 color), $(int column_n)) } |]

-- | Wraps @ImGui::TreeNode()@.
treeNode :: (MonadIO m) => CString -> m Bool
treeNode labelPtr = liftIO do
  (0 /=) <$> [C.exp| bool { TreeNode($(char* labelPtr)) } |]

-- | Wraps @ImGui::TreeNodeEx()@.
treeNodeEx :: (MonadIO m) => CString -> ImGuiTreeNodeFlags -> m Bool
treeNodeEx labelPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { TreeNodeEx($(char* labelPtr), $(ImGuiTreeNodeFlags flags)) } |]

-- | Wraps @ImGui::TreePush()@.
treePush :: (MonadIO m) => CString -> m ()
treePush labelPtr = liftIO do
  [C.exp| void { TreePush($(char* labelPtr)) } |]


-- | Wraps @ImGui::TreePop()@.
treePop :: (MonadIO m) => m ()
treePop = liftIO do
  [C.exp| void { TreePop() } |]

getTreeNodeToLabelSpacing :: (MonadIO m) => m CFloat
getTreeNodeToLabelSpacing = liftIO do
  [C.exp| float { GetTreeNodeToLabelSpacing() } |]

-- | CollapsingHeader returns True when opened but do not indent nor push into the ID stack (because of the `ImGuiTreeNodeFlags_NoTreePushOnOpen` flag).
--
-- This is basically the same as calling `treeNodeEx` with `ImGuiTreeNodeFlags_CollapsingHeader`. You can remove the `ImGuiTreeNodeFlags_NoTreePushOnOpen` flag if you want behavior closer to normal `treeNode`.
--
-- @p_visible == NULL                        @ - regular collapsing header.
-- @p_visible != NULL && *p_visible == true  @ - show a small close button on the corner of the header, clicking the button will set @*p_visible = false@.
-- @p_visible != NULL && *p_visible == false @ - do not show the header at all.
--
-- Do not mistake this with the Open state of the header itself, which you can adjust with SetNextItemOpen() or ImGuiTreeNodeFlags_DefaultOpen.
collapsingHeader :: (MonadIO m) => CString -> Ptr CBool -> ImGuiTreeNodeFlags -> m Bool
collapsingHeader labelPtr visiblePtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { CollapsingHeader($(char* labelPtr), $(bool* visiblePtr), $(ImGuiTreeNodeFlags flags)) } |]

-- | Wraps @ImGui::SetNextItemOpen()@.
setNextItemOpen :: (MonadIO m) => CBool -> m ()
setNextItemOpen is_open = liftIO do
  [C.exp| void { SetNextItemOpen($(bool is_open)) } |]

-- -- | Wraps @ImGui::Selectable()@.
-- selectable :: (MonadIO m) => CString -> m Bool
-- selectable labelPtr = liftIO do
--   (0 /=) <$> [C.exp| bool { Selectable($(char* labelPtr)) } |]


-- | Wraps @ImGui::Selectable()@.
selectable :: (MonadIO m) => CString -> CBool -> ImGuiSelectableFlags -> Ptr ImVec2 -> m Bool
selectable labelPtr selected flags size = liftIO do
  (0 /=) <$> [C.exp| bool { Selectable($(char* labelPtr), $(bool selected), $(ImGuiSelectableFlags flags), *$(ImVec2 *size)) } |]



-- | Wraps @ImGui::ListBox()@.
listBox :: (MonadIO m) => CString -> Ptr CInt -> Ptr CString -> CInt -> m Bool
listBox labelPtr iPtr itemsPtr itemsLen = liftIO do
  (0 /=) <$> [C.exp| bool { ListBox($(char* labelPtr), $(int* iPtr), $(char** itemsPtr), $(int itemsLen)) }|]

-- | Wraps @ImGui::PlotLines()@.
plotLines :: (MonadIO m) => CString -> Ptr CFloat -> CInt -> m ()
plotLines labelPtr valuesPtr valuesLen = liftIO do
  [C.exp| void { PlotLines($(char* labelPtr), $(float* valuesPtr), $(int valuesLen)) } |]

-- | Wraps @ImGui::PlotHistogram()@.
plotHistogram :: (MonadIO m) => CString -> Ptr CFloat -> CInt -> m ()
plotHistogram labelPtr valuesPtr valuesLen = liftIO do
  [C.exp| void { PlotHistogram($(char* labelPtr), $(float* valuesPtr), $(int valuesLen)) } |]


-- | Append to menu-bar of current window (requires 'ImGuiWindowFlagsMenuBar'
-- flag set on parent window).
--
-- Wraps @ImGui::BeginMenuBar()@.
beginMenuBar :: (MonadIO m) => m Bool
beginMenuBar = liftIO do
  (0 /=) <$> [C.exp| bool { BeginMenuBar() } |]


-- | Only call 'endMenuBar' if 'beginMenuBar' returns true!
--
-- Wraps @ImGui::EndMenuBar()@.
endMenuBar :: (MonadIO m) => m ()
endMenuBar = liftIO do
  [C.exp| void { EndMenuBar(); } |]


-- | Create and append to a full screen menu-bar.
--
-- Wraps @ImGui::BeginMainMenuBar()@.
beginMainMenuBar :: (MonadIO m) => m Bool
beginMainMenuBar = liftIO do
  (0 /=) <$> [C.exp| bool { BeginMainMenuBar() } |]


-- | Only call 'endMainMenuBar' if 'beginMainMenuBar' returns true!
--
-- Wraps @ImGui::EndMainMenuBar()@.
endMainMenuBar :: (MonadIO m) => m ()
endMainMenuBar = liftIO do
  [C.exp| void { EndMainMenuBar(); } |]


-- | Create a sub-menu entry.
--
-- Wraps @ImGui::BeginMenu()@.
beginMenu :: (MonadIO m) => CString -> m Bool
beginMenu labelPtr = liftIO do
  (0 /=) <$> [C.exp| bool { BeginMenu($(char* labelPtr)) } |]


-- | Only call 'endMenu' if 'beginMenu' returns true!
--
-- Wraps @ImGui::EndMenu()@.
endMenu :: (MonadIO m) => m ()
endMenu = liftIO do
  [C.exp| void { EndMenu(); } |]


-- | Return true when activated. Shortcuts are displayed for convenience but not
-- processed by ImGui at the moment
--
-- Wraps @ImGui::MenuItem()@
menuItem :: (MonadIO m) => CString -> m Bool
menuItem labelPtr = liftIO do
  (0 /=) <$> [C.exp| bool { MenuItem($(char* labelPtr)) } |]


-- | Create a @TabBar@ and start appending to it.
--
-- Wraps @ImGui::BeginTabBar@.
beginTabBar :: (MonadIO m) => CString -> ImGuiTabBarFlags -> m Bool
beginTabBar tabBarID flags = liftIO do
  (0 /=) <$> [C.exp| bool { BeginTabBar($(char* tabBarID), $(ImGuiTabBarFlags flags) ) } |]


-- | Finish appending elements to a tab bar. Only call if 'beginTabBar' returns @True@.
--
-- Wraps @ImGui::EndTabBar@.
endTabBar :: (MonadIO m) => m ()
endTabBar = liftIO do
  [C.exp| void { EndTabBar(); } |]


-- | Create a new tab. Returns @True@ if the tab is selected.
--
-- Wraps @ImGui::BeginTabItem@.
beginTabItem :: (MonadIO m) => CString -> Ptr CBool -> ImGuiTabItemFlags -> m Bool
beginTabItem namePtr refPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { BeginTabItem($(char* namePtr), $(bool* refPtr), $(ImGuiTabItemFlags flags) ) } |]


-- | Finish appending elements to a tab. Only call if 'beginTabItem' returns @True@.
--
-- Wraps @ImGui::EndTabItem@.
endTabItem :: (MonadIO m) => m ()
endTabItem = liftIO do
  [C.exp| void { EndTabItem(); } |]


-- | Create a tab that behaves like a button. Returns @True@ when clicked. Cannot be selected in the tab bar.
--
-- Wraps @ImGui.TabItemButton@.
tabItemButton :: (MonadIO m) => CString -> ImGuiTabItemFlags -> m Bool
tabItemButton namePtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { TabItemButton($(char* namePtr), $(ImGuiTabItemFlags flags) ) } |]


-- | Notify the tab bar (or the docking system) that a tab/window is about to close.
-- Useful to reduce visual flicker on reorderable tab bars.
--
-- __For tab-bar__: call after 'beginTabBar' and before tab submission. Otherwise, call with a window name.
setTabItemClosed :: (MonadIO m) => CString -> m ()
setTabItemClosed namePtr = liftIO do
  [C.exp| void { SetTabItemClosed($(char* namePtr)); } |]


-- | Begin/append a tooltip window to create full-featured tooltip (with any kind of items).
beginTooltip :: (MonadIO m) => m Bool
beginTooltip = liftIO do
  (0 /=) <$> [C.exp| bool { BeginTooltip() } |]

-- | A shortcut for the @if (IsItemHovered(ImGuiHoveredFlags_ForTooltip) && BeginTooltip())@ idiom.
beginItemTooltip :: (MonadIO m) => m Bool
beginItemTooltip = liftIO do
  (0 /=) <$>  [C.exp| bool { BeginItemTooltip() } |]

-- | Only call if 'beginTooltip'/'beginItemTooltip' returns True!
endTooltip :: (MonadIO m) => m ()
endTooltip = liftIO do
  [C.exp| void { EndTooltip() } |]

-- | Set a text-only tooltip if preceding item was hovered. Overrides any previous call to 'setTooltip'.
setItemTooltip :: (MonadIO m) => CString -> m ()
setItemTooltip textPtr = liftIO do
  [C.exp| void { SetItemTooltip("%s", $(char* textPtr)); } |]

-- | Returns 'True' if the popup is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopup()@
beginPopup :: (MonadIO m) => CString -> m Bool
beginPopup popupIdPtr = liftIO do
  (0 /=) <$> [C.exp| bool { BeginPopup($(char* popupIdPtr)) } |]


-- | Returns 'True' if the modal is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopupModal()@
beginPopupModal :: (MonadIO m) => CString -> m Bool
beginPopupModal popupIdPtr = liftIO do
  (0 /=) <$> [C.exp| bool { BeginPopupModal($(char* popupIdPtr)) } |]


-- | Only call 'endPopup' if 'beginPopup' or 'beginPopupModal' returns 'True'!
--
-- Wraps @ImGui::BeginPopupModal()@
endPopup :: (MonadIO m) => m ()
endPopup = liftIO do
  [C.exp| void { EndPopup() } |]


-- | Call to mark popup as open (don't call every frame!).
--
-- Wraps @ImGui::OpenPopup()@
openPopup :: (MonadIO m) => CString -> m ()
openPopup popupIdPtr = liftIO do
  [C.exp| void { OpenPopup($(char* popupIdPtr)) } |]


-- | Open popup when clicked on last item.
--
-- Note: actually triggers on the mouse _released_ event to be consistent with popup behaviors.
--
-- Wraps @ImGui::OpenPopupOnItemClick()@
openPopupOnItemClick :: (MonadIO m) => CString -> ImGuiPopupFlags-> m ()
openPopupOnItemClick popupIdPtr flags = liftIO do
  [C.exp| void { OpenPopupOnItemClick($(char* popupIdPtr), $(ImGuiPopupFlags flags)) } |]


-- | Manually close the popup we have begin-ed into.
--
-- Wraps @ImGui::ClosePopup()@
closeCurrentPopup :: (MonadIO m) => m ()
closeCurrentPopup = liftIO do
  [C.exp| void { CloseCurrentPopup() } |]

-- | Open+begin popup when clicked on last item.
--
-- Use str_id==NULL to associate the popup to previous item.
--
-- If you want to use that on a non-interactive item such as 'text' you need to pass in an explicit ID here.
beginPopupContextItem :: (MonadIO m) => CString -> ImGuiPopupFlags-> m Bool
beginPopupContextItem popupIdPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { BeginPopupContextItem($(char* popupIdPtr), $(ImGuiPopupFlags flags)) } |]

-- | Open+begin popup when clicked on current window.
beginPopupContextWindow :: (MonadIO m) => CString -> ImGuiPopupFlags-> m Bool
beginPopupContextWindow popupIdPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { BeginPopupContextWindow($(char* popupIdPtr), $(ImGuiPopupFlags flags)) } |]

-- | Open+begin popup when clicked in void (where there are no windows).
beginPopupContextVoid :: (MonadIO m) => CString -> ImGuiPopupFlags-> m Bool
beginPopupContextVoid popupIdPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { BeginPopupContextVoid($(char* popupIdPtr), $(ImGuiPopupFlags flags)) } |]

-- | Query popup status
--
-- - return 'True' if the popup is open at the current 'beginPopup' level of the popup stack.
-- - with 'ImGuiPopupFlags_AnyPopupId': return 'True' if any popup is open at the current 'beginPopup' level of the popup stack.
-- - with 'ImGuiPopupFlags_AnyPopupId' | 'ImGuiPopupFlags_AnyPopupLevel': return 'True' if any popup is open.
--
-- Wraps @ImGui::IsPopupOpen()@
isPopupOpen :: (MonadIO m) => CString -> ImGuiPopupFlags-> m Bool
isPopupOpen popupIdPtr flags = liftIO do
  (0 /=) <$> [C.exp| bool { IsPopupOpen($(char* popupIdPtr), $(ImGuiPopupFlags flags)) } |]


-- | Is the last item hovered? (and usable, aka not blocked by a popup, etc.).
--
-- Wraps @ImGui::IsItemHovered()@
isItemHovered :: (MonadIO m) => m Bool
isItemHovered = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemHovered() } |]

-- | Is the last item hovered? (and usable, aka not blocked by a popup, etc.).
isItemActive :: (MonadIO m) => m Bool
isItemActive = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemActive() } |]

isItemFocused :: (MonadIO m) => m Bool
isItemFocused = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemFocused() } |]

isItemClicked :: (MonadIO m) => ImGuiMouseButton -> m Bool
isItemClicked mouseButton = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemClicked( $(ImGuiMouseButton mouseButton) ) } |]

isItemVisible :: (MonadIO m) => m Bool
isItemVisible = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemVisible() } |]

isItemEdited :: (MonadIO m) => m Bool
isItemEdited = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemEdited() } |]

isItemActivated :: (MonadIO m) => m Bool
isItemActivated = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemActivated() } |]

isItemDeactivated :: (MonadIO m) => m Bool
isItemDeactivated = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemDeactivated() } |]

isItemDeactivatedAfterEdit :: (MonadIO m) => m Bool
isItemDeactivatedAfterEdit = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemDeactivatedAfterEdit() } |]

isItemToggledOpen :: (MonadIO m) => m Bool
isItemToggledOpen = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemToggledOpen() } |]

isAnyItemHovered :: (MonadIO m) => m Bool
isAnyItemHovered = liftIO do
  (0 /=) <$> [C.exp| bool { IsAnyItemHovered() } |]

-- | Is the last item hovered? (and usable, aka not blocked by a popup, etc.).
isAnyItemActive :: (MonadIO m) => m Bool
isAnyItemActive = liftIO do
  (0 /=) <$> [C.exp| bool { IsAnyItemActive() } |]

isAnyItemFocused :: (MonadIO m) => m Bool
isAnyItemFocused = liftIO do
  (0 /=) <$> [C.exp| bool { IsAnyItemFocused() } |]

getItemID :: (MonadIO m) => m ImGuiID
getItemID = liftIO do
  [C.exp| ImGuiID { GetItemID() } |]

getItemRectMin :: (MonadIO m) => m ImVec2
getItemRectMin = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetItemRectMin();
      }
    |]

getItemRectMax :: (MonadIO m) => m ImVec2
getItemRectMax = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetItemRectMin();
      }
    |]

getItemRectSize :: (MonadIO m) => m ImVec2
getItemRectSize = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetItemRectSize();
      }
    |]

-- | Get draw list associated to the current window.
getWindowDrawList :: (MonadIO m) => m DrawList
getWindowDrawList = liftIO do
  DrawList <$> [C.exp|
    ImDrawList* {
      GetWindowDrawList()
    }
  |]

-- | Get current window position in screen space.
--
-- Useful if you want to do your own drawing via the "DrawList" API.
getWindowPos :: (MonadIO m) => m ImVec2
getWindowPos = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetWindowPos();
      }
    |]

getWindowSize :: (MonadIO m) => m ImVec2
getWindowSize = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetWindowSize();
      }
    |]

getWindowWidth :: (MonadIO m) => m CFloat
getWindowWidth = liftIO do
  [C.exp| float { GetWindowWidth() } |]

getWindowHeight :: (MonadIO m) => m CFloat
getWindowHeight = liftIO do
  [C.exp| float { GetWindowHeight() } |]

-- | Set next window position. Call before `begin` Use pivot=(0.5,0.5) to center on given point, etc.
--
-- Wraps @ImGui::SetNextWindowPos()@
setNextWindowPos :: (MonadIO m) => Ptr ImVec2 -> ImGuiCond -> Maybe (Ptr ImVec2) -> m ()
setNextWindowPos posPtr cond (Just pivotPtr) = liftIO do
  [C.exp| void { SetNextWindowPos(*$(ImVec2* posPtr), $(ImGuiCond cond), *$(ImVec2* pivotPtr)) } |]
setNextWindowPos posPtr cond Nothing = liftIO do
  [C.exp| void { SetNextWindowPos(*$(ImVec2* posPtr), $(ImGuiCond cond)) } |]


-- | Set next window size. Call before `begin`
--
-- Wraps @ImGui::SetNextWindowSize()@
setNextWindowSize :: (MonadIO m) => Ptr ImVec2 -> ImGuiCond -> m ()
setNextWindowSize sizePtr cond = liftIO do
  [C.exp| void { SetNextWindowSize(*$(ImVec2* sizePtr), $(ImGuiCond cond)) } |]


-- | Set next window size and position to match current display size.
--
-- Call before `begin`.
--
-- Wraps @ImGui::SetNextWindowPos()@, @ImGui::SetNextWindowSize()@
setNextWindowFullscreen :: (MonadIO m) => m ()
setNextWindowFullscreen = liftIO
  [C.block|
    void {
      SetNextWindowPos(ImVec2(0, 0));
      SetNextWindowSize(GetIO().DisplaySize);
    }
  |]

-- | Set next window content size (~ scrollable client area, which enforce the range of scrollbars). Not including window decorations (title bar, menu bar, etc.) nor WindowPadding. call before `begin`
--
-- Wraps @ImGui::SetNextWindowContentSize()@
setNextWindowContentSize :: (MonadIO m) => Ptr ImVec2 -> m ()
setNextWindowContentSize sizePtr = liftIO do
  [C.exp| void { SetNextWindowContentSize(*$(ImVec2* sizePtr)) } |]


-- | Set next window size limits. use -1,-1 on either X/Y axis to preserve the current size. Sizes will be rounded down.
--
-- Wraps @ImGui::SetNextWindowContentSize()@
setNextWindowSizeConstraints :: (MonadIO m) => Ptr ImVec2 -> Ptr ImVec2 -> m ()
setNextWindowSizeConstraints sizeMinPtr sizeMaxPtr = liftIO do
  [C.exp| void { SetNextWindowSizeConstraints(*$(ImVec2* sizeMinPtr), *$(ImVec2* sizeMaxPtr)) } |]

-- | Set next window collapsed state. call before `begin`
--
-- Wraps @ImGui::SetNextWindowCollapsed()@
setNextWindowCollapsed :: (MonadIO m) => CBool -> ImGuiCond -> m ()
setNextWindowCollapsed b cond = liftIO do
  [C.exp| void { SetNextWindowCollapsed($(bool b), $(ImGuiCond cond)) } |]

-- | Set next window to be focused / top-most. call before `begin`
setNextWindowFocus :: (MonadIO m) => m ()
setNextWindowFocus = liftIO do
  [C.exp| void { SetNextWindowFocus() } |]

-- | Set next window scrolling value (use < 0.0f to not affect a given axis).
setNextWindowScroll :: (MonadIO m) => Ptr ImVec2 -> m ()
setNextWindowScroll posPtr = liftIO do
  [C.exp| void { SetNextWindowScroll(*$(ImVec2* posPtr)) } |]

-- | Set next window background color alpha. helper to easily override the Alpha component of `ImGuiCol_WindowBg`, `ChildBg`, `PopupBg`. you may also use `ImGuiWindowFlags_NoBackground`.
--
-- Wraps @ImGui::SetNextWindowBgAlpha()@
setNextWindowBgAlpha :: (MonadIO m) => CFloat -> m ()
setNextWindowBgAlpha alpha = liftIO do
  [C.exp| void { SetNextWindowBgAlpha($(float alpha)) } |]

-- | Retrieve available space from a given point.
--
-- @== GetContentRegionMax() - GetCursorPos()@
getContentRegionAvail :: (MonadIO m) => m ImVec2
getContentRegionAvail = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetContentRegionAvail();
      }
    |]

-- | Current content boundaries (typically window boundaries including scrolling, or current column boundaries), in window coordinates.
getContentRegionMax :: (MonadIO m) => m ImVec2
getContentRegionMax = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetContentRegionMax();
      }
    |]

-- | Content boundaries min for the full window (roughly @(0,0) - Scroll@), in window coordinates.
getWindowContentRegionMin :: (MonadIO m) => m ImVec2
getWindowContentRegionMin = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetWindowContentRegionMin();
      }
    |]

-- | Content boundaries max for the full window (roughly @(0,0) + Size - Scroll@) where Size can be overridden with SetNextWindowContentSize(), in window coordinates.
getWindowContentRegionMax :: (MonadIO m) => m ImVec2
getWindowContentRegionMax = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetWindowContentRegionMax();
      }
    |]

-- | Begin a block that may be disabled. This disables all user interactions
-- and dims item visuals.
--
-- Always call a matching 'endDisabled' for each 'beginDisabled' call.
--
-- The boolean argument is only intended to facilitate use of boolean
-- expressions. If you can avoid calling @beginDisabled 0@ altogether,
-- that should be preferred.
--
-- Wraps @ImGui::BeginDisabled()@
beginDisabled :: (MonadIO m) => CBool -> m ()
beginDisabled disabled = liftIO do
  [C.exp| void { BeginDisabled($(bool disabled)) } |]


-- | Ends a block that may be disabled.
--
-- Wraps @ImGui::EndDisabled()@
endDisabled :: (MonadIO m) => m ()
endDisabled = liftIO do
  [C.exp| void { EndDisabled() } |]

-- | Make last item the default focused item of a window.
setItemDefaultFocus :: (MonadIO m) => m ()
setItemDefaultFocus = liftIO do
  [C.block| void { SetItemDefaultFocus(); } |]

-- | Focus keyboard on the next widget.
--
-- Use positive 'offset' to access sub components of a multiple component widget.
-- Use -1 to access previous widget.
setKeyboardFocusHere :: (MonadIO m) => CInt -> m ()
setKeyboardFocusHere offset = liftIO do
  [C.block| void { SetKeyboardFocusHere( $(int offset)); } |]

-- | Allow next item to be overlapped by a subsequent item.
--
-- Useful with invisible buttons, selectable, treenode covering an area where subsequent items may need to be added.
-- Note that both `selectable` and `treeNode` have dedicated flags doing this.
setNextItemAllowOverlap :: (MonadIO m) => m ()
setNextItemAllowOverlap = liftIO do
  [C.block| void { SetNextItemAllowOverlap(); } |]

-- | undo a sameLine or force a new line when in an horizontal-layout context.
--
-- Wraps @ImGui::NewLine()@
newLine :: (MonadIO m) => m ()
newLine = liftIO do
  [C.exp| void { NewLine() } |]


-- | Add vertical spacing.
--
-- Wraps @ImGui::Spacing()@
spacing :: (MonadIO m) => m ()
spacing = liftIO do
  [C.exp| void { Spacing() } |]


-- | Add a dummy item of given size. unlike `invisibleButton`, `dummy` won't take the mouse click or be navigable into.
--
-- Wraps @ImGui::Dummy()@
dummy :: (MonadIO m) => Ptr ImVec2 -> m ()
dummy sizePtr = liftIO do
  [C.exp| void { Dummy(*$(ImVec2* sizePtr)) } |]


-- | Move content position toward the right, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Indent()@
indent :: (MonadIO m) => CFloat -> m ()
indent indent_w = liftIO do
  [C.exp| void { Indent($(float indent_w)) } |]


-- | Move content position back to the left, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Unindent()@
unindent :: (MonadIO m) => CFloat -> m ()
unindent indent_w = liftIO do
  [C.exp| void { Unindent($(float indent_w)) } |]


-- | Affect large frame+labels widgets only.
--
-- Wraps @ImGui::SetNextItemWidth()@
setNextItemWidth :: (MonadIO m) => CFloat -> m ()
setNextItemWidth itemWidth = liftIO do
  [C.exp| void { SetNextItemWidth($(float itemWidth)) } |]


-- Wraps @ImGui::PushItemWidth()@
pushItemWidth :: (MonadIO m) => CFloat -> m ()
pushItemWidth itemWidth = liftIO do
  [C.exp| void { PushItemWidth($(float itemWidth)) } |]


-- Wraps @ImGui::PopItemWidth()@
popItemWidth :: (MonadIO m) => m ()
popItemWidth = liftIO do
  [C.exp| void { PopItemWidth() } |]

-- | Width of item given pushed settings and current cursor position. NOT necessarily the width of last item unlike most Item functions.
calcItemWidth :: MonadIO m => m Float
calcItemWidth = liftIO do
  realToFrac <$> [C.exp| float { CalcItemWidth() } |]

-- | Push word-wrapping position for Text commands.
--
-- Negative: no wrapping.
-- Zero: wrap to end of window (or column).
-- Positive: wrap at 'wrap_pos_x' position in window local space.
pushTextWrapPos :: (MonadIO m) => CFloat -> m ()
pushTextWrapPos wrapLocalPosX = liftIO do
  [C.exp| void { PushTextWrapPos($(float wrapLocalPosX)) } |]

popTextWrapPos :: (MonadIO m) => m ()
popTextWrapPos = liftIO do
  [C.exp| void { PopTextWrapPos() } |]

-- | lock horizontal starting position
--
--  Wraps @ImGui::BeginGroup()@
beginGroup :: (MonadIO m) => m ()
beginGroup = liftIO do
  [C.exp| void { BeginGroup() } |]


-- | unlock horizontal starting position + capture the whole group bounding box into one "item" (so you can use `isItemHovered` or layout primitives such as `sameLine` on whole group, etc.)
--
-- Wraps @ImGui::EndGroup()@
endGroup :: (MonadIO m) => m ()
endGroup = liftIO do
  [C.exp| void { EndGroup() } |]


-- | Vertically align upcoming text baseline to FramePadding.y so that it will align properly to regularly framed items (call if you have text on a line before a framed item)
--
-- Wraps @ImGui::AlignTextToFramePadding()@
alignTextToFramePadding :: (MonadIO m) => m ()
alignTextToFramePadding = liftIO do
  [C.exp| void { AlignTextToFramePadding() } |]

-- | ~ @FontSize@
getTextLineHeight :: (MonadIO m) => m CFloat
getTextLineHeight = liftIO do
  [C.exp| float { GetTextLineHeight() } |]

-- | Distance in pixels between 2 consecutive lines of text.
--
-- ~ @FontSize + style.ItemSpacing.y@ ()
getTextLineHeightWithSpacing :: (MonadIO m) => m CFloat
getTextLineHeightWithSpacing = liftIO do
  [C.exp| float { GetTextLineHeightWithSpacing() } |]

-- | ~ @FontSize + style.FramePadding.y * 2@
getFrameHeight :: (MonadIO m) => m CFloat
getFrameHeight = liftIO do
  [C.exp| float { GetFrameHeight() } |]

-- | Distance in pixels between 2 consecutive lines of framed widgets
--
-- ~ @FontSize + style.FramePadding.y * 2 + style.ItemSpacing.y@
getFrameHeightWithSpacing :: (MonadIO m) => m CFloat
getFrameHeightWithSpacing = liftIO do
  [C.exp| float { GetFrameHeightWithSpacing() } |]

-- | Set cursor position in window-local coordinates
--
-- Wraps @ImGui::SetCursorPos()@
setCursorPos :: (MonadIO m) => Ptr ImVec2 -> m ()
setCursorPos posPtr = liftIO do
  [C.exp| void { SetCursorPos(*$(ImVec2* posPtr)) } |]

setCursorPosX :: (MonadIO m) => CFloat -> m ()
setCursorPosX localX = liftIO do
  [C.exp| void { SetCursorPosX($(float localX)) } |]

setCursorPosY :: (MonadIO m) => CFloat -> m ()
setCursorPosY localY = liftIO do
  [C.exp| void { SetCursorPosY($(float localY)) } |]

-- | Get cursor position in window-local coordinates.
--
-- Useful to overlap draw using 'setCursorPos'.
--
-- Wraps @ImGui::SetCursorPos()@
getCursorPos :: (MonadIO m) => m ImVec2
getCursorPos = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetCursorPos();
      }
    |]

getCursorPosX :: (MonadIO m) => m CFloat
getCursorPosX = liftIO do
  [C.exp| float { GetCursorPosX() } |]

getCursorPosY :: (MonadIO m) => m CFloat
getCursorPosY = liftIO do
  [C.exp| float { GetCursorPosY() } |]

-- | Cursor position in absolute coordinates.
--
-- Useful to work with 'DrawList' API.
--
-- Generally top-left == @GetMainViewport()->Pos == (0,0)@ in single viewport mode,
-- and bottom-right == @GetMainViewport()->Pos+Size == io.DisplaySize@ in single-viewport mode.
getCursorScreenPos :: (MonadIO m) => m ImVec2
getCursorScreenPos = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetCursorScreenPos();
      }
    |]

-- | Initial cursor position, in window coordinates.
getCursorStartPos :: (MonadIO m) => m ImVec2
getCursorStartPos = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetCursorStartPos();
      }
    |]

-- | Set cursor position in absolute coordinates.
setCursorScreenPos :: (MonadIO m) => Ptr ImVec2 -> m ()
setCursorScreenPos posPtr = liftIO do
  [C.exp| void { SetCursorScreenPos(*$(ImVec2* posPtr)) } |]


-- | Modify a style color by pushing to the shared stack. always use this if you modify the style after `newFrame`
--
-- Wraps @ImGui::PushStyleColor()@
pushStyleColor :: (MonadIO m) => ImGuiCol -> Ptr ImVec4 -> m ()
pushStyleColor col colorPtr = liftIO do
  [C.exp| void { PushStyleColor($(ImGuiCol col), *$(ImVec4 *colorPtr)) } |]


-- | Remove style color modifications from the shared stack
--
-- Wraps @ImGui::PopStyleColor()@
popStyleColor :: (MonadIO m) => CInt -> m ()
popStyleColor n = liftIO do
  [C.exp| void { PopStyleColor($(int n)) } |]


-- | Modify a style variable by pushing to the shared stack. always use this if you modify the style after `newFrame`
--
-- Wraps @ImGui::PushStyleVar()@
pushStyleVar :: (MonadIO m) => ImGuiStyleVar -> Ptr ImVec2 -> m ()
pushStyleVar style valPtr = liftIO do
  [C.exp| void { PushStyleVar($(ImGuiStyleVar style), *$(ImVec2* valPtr)) } |]


-- | Remove style variable modifications from the shared stack
--
-- Wraps @ImGui::PopStyleVar()@
popStyleVar :: (MonadIO m) => CInt -> m ()
popStyleVar n = liftIO do
  [C.exp| void { PopStyleVar($(int n)) } |]

-- | Allow/disable focusing using TAB/Shift-TAB, enabled by default but you can disable it for certain widgets.
pushTabStop :: (MonadIO m) => CBool -> m ()
pushTabStop b = liftIO do
  [C.exp| void { PushTabStop($(bool b)) } |]

popTabStop :: (MonadIO m) => m ()
popTabStop = liftIO do
  [C.exp| void { PopTabStop() } |]

-- | Push integer into the ID stack (will hash int).
--
-- Wraps @ImGui::PushId@
pushIDInt :: (MonadIO m) => CInt -> m ()
pushIDInt intId = liftIO do
  [C.exp| void { PushID($(int intId)) } |]

-- | Push pointer into the ID stack (will hash pointer).
--
-- Wraps @ImGui::PushId@
pushIDPtr :: (MonadIO m) => Ptr a -> m ()
pushIDPtr ptr = liftIO do
  [C.exp| void { PushID($(void * ptr_)) } |]
  where
    ptr_ = castPtr ptr

-- | Push string into the ID stack (will hash string).
--
-- Wraps @ImGui::PushId@
pushIDStr :: (MonadIO m) => CString -> m ()
pushIDStr strId = liftIO do
  [C.exp| void { PushID($(char * strId)) } |]

-- | Push string into the ID stack (will hash string).
--
-- Wraps @ImGui::PushId@
pushIDStrLen :: (MonadIO m) => CStringLen -> m ()
pushIDStrLen (strBegin, strLen) = liftIO do
  [C.exp| void { PushID($(char * strBegin), $(char * strEnd)) } |]
  where
    strEnd = plusPtr strBegin strLen

popID :: (MonadIO m) => m ()
popID = liftIO do
  [C.exp| void { PopID() } |]

getMousePos :: (MonadIO m) => m ImVec2
getMousePos = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetMousePos();
      }
    |]

-- | Retrieve mouse position at the time of opening popup we have 'beginPopup' into (helper to avoid user backing that value themselves).
getMousePosOnOpeningCurrentPopup :: (MonadIO m) => m ImVec2
getMousePosOnOpeningCurrentPopup = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetMousePosOnOpeningCurrentPopup();
      }
    |]

isMouseDragging :: MonadIO m => ImGuiMouseButton -> CFloat -> m Bool
isMouseDragging btn lockThreshold = liftIO do
  (0 /=) <$> [C.exp| bool { IsMouseDragging( $(ImGuiMouseButton btn), $(float lockThreshold) ) } |]

getMouseDragDelta :: (MonadIO m) => ImGuiMouseButton -> CFloat -> m ImVec2
getMouseDragDelta btn lockThreshold = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = GetMouseDragDelta( $(ImGuiMouseButton btn), $(float lockThreshold) );
      }
    |]

resetMouseDragDelta :: MonadIO m => ImGuiMouseButton -> m ()
resetMouseDragDelta btn = liftIO do
  [C.block| void { ResetMouseDragDelta( $(ImGuiMouseButton btn) ); } |]

wantCaptureMouse :: MonadIO m => m Bool
wantCaptureMouse = liftIO do
  (0 /=) <$> [C.exp| bool { GetIO().WantCaptureMouse } |]

type ImGuiKeyChord = Int

shortcut :: MonadIO m => ImGuiKeyChord -> ImGuiInputFlags -> m Bool
shortcut keyChord flags = liftIO do
  (0 /=) <$> [C.exp| bool { Shortcut( $(ImGuiKeyChord keyChord), $(ImGuiInputFlags flags) ) } |]

setNextItemShortcut :: MonadIO m => ImGuiKeyChord -> ImGuiInputFlags -> m ()
setNextItemShortcut keyChord flags = liftIO do
  [C.block| void { SetNextItemShortcut( $(ImGuiKeyChord keyChord), $(ImGuiInputFlags flags) ); } |]

wantCaptureKeyboard :: MonadIO m => m Bool
wantCaptureKeyboard = liftIO do
  (0 /=) <$> [C.exp| bool { GetIO().WantCaptureKeyboard } |]

-- | Estimate of application framerate (rolling average over 60 frames), in
-- frame per second. Solely for convenience.
framerate :: MonadIO m => m Float
framerate = liftIO do
  realToFrac <$> [C.exp| float { GetIO().Framerate } |]

-- | Get global imgui time.
--
-- Incremented by io.DeltaTime every frame.
getTime :: MonadIO m => m Double
getTime = liftIO do
  realToFrac <$> [C.exp| double { GetTime() } |]

-- | Get global imgui frame count.
--
-- Incremented by 1 every frame.
getFrameCount :: MonadIO m => m Int
getFrameCount = liftIO do
  fromIntegral <$> [C.exp| int { GetFrameCount() } |]

calcTextSize :: MonadIO m => CString -> CString -> CBool -> CFloat -> m ImVec2
calcTextSize textPtr textEndPtr hideAfterDoubleHash wrapWidth = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = CalcTextSize(
          $(char* textPtr),
          $(char* textEndPtr),
          $(bool hideAfterDoubleHash),
          $(float wrapWidth)
        );
      }
    |]

-- | This draw list will be the first rendering one.
--
-- Useful to quickly draw shapes/text behind dear imgui contents.
getBackgroundDrawList :: (MonadIO m) => m DrawList
getBackgroundDrawList = liftIO do
  DrawList <$> [C.exp|
    ImDrawList* {
      GetBackgroundDrawList()
    }
  |]

--  | This draw list will be the last rendered one.
--
-- Useful to quickly draw shapes/text over dear imgui contents.
getForegroundDrawList :: (MonadIO m) => m DrawList
getForegroundDrawList = liftIO do
  DrawList <$> [C.exp|
    ImDrawList* {
      GetForegroundDrawList()
    }
  |]

-- | Generate 32-bit encoded colors using DearImgui macros.
--
-- Follows @IMGUI_USE_BGRA_PACKED_COLOR@ define to put bytes in appropriate positions.
imCol32 :: CUChar -> CUChar -> CUChar -> CUChar -> ImU32
imCol32 r g b a = unsafePerformIO
  [C.exp|
    ImU32 {
      IM_COL32(
        $(unsigned char r),
        $(unsigned char g),
        $(unsigned char b),
        $(unsigned char a)
      )
    }
  |]
