{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: DearImGui

Main ImGui module, exporting the functions to create a GUI.

Enum values live in the per-enum modules of @dear-imgui-raw@, e.g.
@import qualified DearImGui.Raw.Enums.ImGuiWindowFlags as ImGuiWindowFlags@
gives @ImGuiWindowFlags.NoTitleBar@.
-}

module DearImGui
  ( -- * Context Creation and Access
    createContext
  , destroyContext
  , getCurrentContext
  , setCurrentContext

    -- * Main
  , newFrame
  , endFrame
  , render
  , getDrawData
  , checkVersion

    -- * Configuration
  , enableKeyboardNav
  , disableKeyboardNav
  , isKeyboardNavEnabled

    -- * Demo, Debug, Information
  , showDemoWindow
  , showIDStackToolWindow
  , showMetricsWindow
  , showAboutWindow
  , showStyleSelector
  , showFontSelector
  , showUserGuide
  , getVersion

    -- * Logging
  , showDebugLogWindow
  , logButtons

    -- * Styles
  , styleColorsDark
  , styleColorsLight
  , styleColorsClassic

    -- * Windows
  , withWindow
  , withWindowOpen
  , withCloseableWindow
  , withFullscreen
  , fullscreenFlags

  , begin
  , beginWithClose
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

    -- ** Child Windows
  , withChild
  , withChildOpen
  , withChildContext
  , beginChild
  , endChild

    -- * Parameter stacks
  , withStyleColor
  , pushStyleColor
  , popStyleColor

  , withStyleVar
  , pushStyleVar
  , popStyleVar
  , withTabStop
  , pushTabStop
  , popTabStop

  , withFont
  , withFontWithSize
  , withFontLegacySize
  , withFontSize
  , pushFont
  , pushFontWithSize
  , pushFontLegacySize
  , pushFontSize
  , popFont

    -- * Cursor/Layout
  , separator
  , sameLine
  , newLine
  , spacing
  , dummy

  , withIndent
  , indent
  , unindent

  , setNextItemWidth
  , withItemWidth
  , pushItemWidth
  , popItemWidth
  , calcItemWidth
  , withTextWrapPos
  , pushTextWrapPos
  , popTextWrapPos

  , withGroup
  , beginGroup
  , endGroup

  , setCursorPos
  , setCursorPosX
  , setCursorPosY
  , setCursorScreenPos
  , getCursorPos
  , getCursorPosX
  , getCursorPosY
  , getCursorStartPos
  , alignTextToFramePadding
  , getTextLineHeight
  , getTextLineHeightWithSpacing
  , getFrameHeight
  , getFrameHeightWithSpacing

    -- * ID stack
  , withID
  , ToID(..)

    -- * Widgets
    -- ** Text
  , text
  , textColored
  , textDisabled
  , textWrapped
  , labelText
  , bulletText
  , separatorText
  , valueBool
  , valueFloat
  , valueInt32
  , valueWord32

    -- ** Main
  , button
  , smallButton
  , invisibleButton
  , arrowButton
  , image
  , imageWithBg
  , imageButton
  , checkbox
  , checkboxFlags
  , checkboxFlagsU
  , radioButton
  , radioButtonI
  , progressBar
  , bullet

    -- ** Combo Box
  , withCombo
  , withComboOpen
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
  , inputPassword
  , inputFloat
  , inputFloat2
  , inputFloat3
  , inputFloat4
  , inputInt
  , inputInt2
  , inputInt3
  , inputInt4
  , inputScalar
  , inputScalarN

    -- ** Color Editor/Picker
  , colorEdit3
  , colorEdit4
  , colorPicker3
  , colorPicker4
  , colorButton

    -- ** Tables
  , withTable
  , withTableOpen
  , TableOptions(..)
  , defTableOptions
  , beginTable
  , endTable

    -- *** Setup
  , tableSetupColumn
  , tableSetupColumnWith
  , TableColumnOptions(..)
  , defTableColumnOptions

  , tableHeadersRow
  , tableHeader
  , tableSetupScrollFreeze

    -- *** Rows
  , tableNextRow
  , tableNextRowWith
  , TableRowOptions(..)
  , defTableRowOptions

    -- *** Columns
  , tableNextColumn
  , tableSetColumnIndex

    -- *** Sorting
  , withSortableTable
  , TableSortingSpecs(..)

    -- *** Queries
  , tableGetColumnCount
  , tableGetColumnIndex
  , tableGetRowIndex
  , tableGetColumnName
  , tableGetColumnFlags
  , tableSetColumnEnabled
  , tableSetBgColor

    -- ** Trees
  , treeNode
  , treeNodeWith
  , treePush
  , treePop
  , setNextItemOpen
  , collapsingHeader
  , getTreeNodeToLabelSpacing

    -- ** Selectables
  , selectable
  , selectableWith
  , SelectableOptions(..)
  , defSelectableOptions

    -- ** List Boxes
  , listBox

    -- ** Data Plotting
  , plotLines
  , plotHistogram

    -- ** Menus
  , withMenuBar
  , withMenuBarOpen
  , beginMenuBar
  , endMenuBar

  , withMainMenuBar
  , withMainMenuBarOpen
  , beginMainMenuBar
  , endMainMenuBar

  , withMenu
  , withMenuOpen
  , beginMenu
  , endMenu

  , menuItem

    -- ** Tabs, tab bar
  , withTabBar
  , withTabBarOpen
  , beginTabBar
  , endTabBar

  , withTabItem
  , withTabItemOpen
  , beginTabItem
  , endTabItem
  , tabItemButton
  , setTabItemClosed

    -- ** Tooltips
  , setItemTooltip
  , withItemTooltip
  , withTooltip
  , beginTooltip
  , beginItemTooltip
  , endTooltip

    -- ** Disabled blocks
  , withDisabled
  , beginDisabled
  , endDisabled

    -- * Popups/Modals

    -- ** Generic
  , withPopup
  , withPopupOpen
  , beginPopup
  , endPopup

    -- ** Modal
  , withPopupModal
  , withPopupModalOpen
  , beginPopupModal

    -- ** Item context
  , itemContextPopup
  , withPopupContextItemOpen
  , withPopupContextItem
  , beginPopupContextItem

    -- ** Window context
  , windowContextPopup
  , withPopupContextWindowOpen
  , withPopupContextWindow
  , beginPopupContextWindow

    -- ** Void context
  , voidContextPopup
  , withPopupContextVoidOpen
  , withPopupContextVoid
  , beginPopupContextVoid

    -- ** Manual
  , openPopup
  , openPopupOnItemClick
  , closeCurrentPopup

    -- ** Queries
  , isCurrentPopupOpen
  , isAnyPopupOpen
  , isAnyLevelPopupOpen

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

    -- * Utilities
  , wantCaptureMouse
  , getMousePos
  , getMousePosOnOpeningCurrentPopup
  , isMouseDragging
  , getMouseDragDelta
  , resetMouseDragDelta

  , wantCaptureKeyboard
  , shortcut
  , setNextItemShortcut

  , setItemDefaultFocus
  , setKeyboardFocusHere
  , setNextItemAllowOverlap

   -- ** Drag and drop
  , withDragDropSource
  , withDragDropTarget
  , withDragDropSource_
  , withDragDropTarget_
  , withDragDropSourceData
  , withDragDropTargetData

    -- ** ListClipper
  , withListClipper
  , ClipItems(..)
  , ClipRange(..)

    -- ** Miscellaneous
  , getBackgroundDrawList
  , getForegroundDrawList
  , imCol32
  , framerate
  , getTime
  , getFrameCount
  , calcTextSize

    -- * Types
  , module DearImGui.Enums
  , module DearImGui.Structs
  )
  where

-- base
import Control.Monad
  ( unless, when, void )
import Data.Bool
import Data.Foldable
  ( foldl', traverse_ )
import Data.Maybe
  ( fromMaybe )
import Foreign hiding (void)
import Foreign.C
import Text.Printf
  ( printf )

-- dear-imgui
import DearImGui.Enums
import DearImGui.Internal.Text (Text)
import DearImGui.Structs
import qualified DearImGui.Internal.Text as Text

-- dear-imgui-raw
import DearImGui.Raw
  ( ImDrawIdx
  , pattern IM_COL32_A_SHIFT
  , pattern IM_COL32_B_SHIFT
  , pattern IM_COL32_G_SHIFT
  , pattern IM_COL32_R_SHIFT
  )
import DearImGui.Raw.ImDrawVert (ImDrawVert)
import qualified DearImGui.Raw.Enums.ImGuiChildFlags as ImGuiChildFlags
import qualified DearImGui.Raw.Enums.ImGuiCond as ImGuiCond
import qualified DearImGui.Raw.Enums.ImGuiConfigFlags as ImGuiConfigFlags
import qualified DearImGui.Raw.Enums.ImGuiInputTextFlags as ImGuiInputTextFlags
import qualified DearImGui.Raw.Enums.ImGuiItemFlags as ImGuiItemFlags
import qualified DearImGui.Raw.Enums.ImGuiPopupFlags as ImGuiPopupFlags
import qualified DearImGui.Raw.Enums.ImGuiSliderFlags as ImGuiSliderFlags
import qualified DearImGui.Raw.Enums.ImGuiSortDirection as ImGuiSortDirection
import qualified DearImGui.Raw.Enums.ImGuiWindowFlags as ImGuiWindowFlags
import qualified DearImGui.Raw.ImGui as ImGui
import qualified DearImGui.Raw.ImGuiListClipper as ListClipper

-- managed
import qualified Control.Monad.Managed as Managed

-- StateVar
import Data.StateVar
  ( HasGetter(get), HasSetter, ($=!) )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- unliftio
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Exception (bracket, bracket_)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

-- | Wraps @ImGui::CreateContext()@.
{-# INLINE createContext #-}
createContext :: MonadIO m => m Context
createContext = liftIO $ ImGui.createContext nullPtr

-- | Wraps @ImGui::DestroyContext()@.
{-# INLINE destroyContext #-}
destroyContext :: MonadIO m => Context -> m ()
destroyContext = liftIO . ImGui.destroyContext

-- | Wraps @ImGui::GetCurrentContext()@.
{-# INLINE getCurrentContext #-}
getCurrentContext :: MonadIO m => m Context
getCurrentContext = liftIO ImGui.getCurrentContext

-- | Wraps @ImGui::SetCurrentContext()@.
{-# INLINE setCurrentContext #-}
setCurrentContext :: MonadIO m => Context -> m ()
setCurrentContext = liftIO . ImGui.setCurrentContext

-- | Start a new Dear ImGui frame, you can submit any command from this point
-- until 'render'/'endFrame'.
--
-- Wraps @ImGui::NewFrame()@.
{-# INLINE newFrame #-}
newFrame :: MonadIO m => m ()
newFrame = liftIO ImGui.newFrame

-- | Ends the Dear ImGui frame. automatically called by 'render'. If you don't
-- need to render data (skipping rendering) you may call 'endFrame' without
-- 'render'... but you'll have wasted CPU already! If you don't need to render,
-- better to not create any windows and not call 'newFrame' at all!
{-# INLINE endFrame #-}
endFrame :: MonadIO m => m ()
endFrame = liftIO ImGui.endFrame

-- | Ends the Dear ImGui frame, finalize the draw data. You can then get call
-- 'getDrawData'.
{-# INLINE render #-}
render :: MonadIO m => m ()
render = liftIO ImGui.render

-- | Valid after 'render' and until the next call to 'newFrame'. This is what
-- you have to render.
{-# INLINE getDrawData #-}
getDrawData :: MonadIO m => m DrawData
getDrawData = liftIO ImGui.getDrawData

-- | Verify that the compiled library matches the version and data layout of
-- the headers the bindings were built against.
--
-- Wraps @IMGUI_CHECKVERSION()@.
{-# INLINE checkVersion #-}
checkVersion :: MonadIO m => m ()
checkVersion = liftIO do
  version <- ImGui.getVersion
  ok <- ImGui.debugCheckVersionAndDataLayout
    version
    (sizeOfC (undefined :: ImGuiIO))
    (sizeOfC (undefined :: ImGuiStyle))
    (sizeOfC (undefined :: ImVec2))
    (sizeOfC (undefined :: ImVec4))
    (sizeOfC (undefined :: ImDrawVert))
    (sizeOfC (undefined :: ImDrawIdx))
  when (ok == 0) $
    fail "DearImGui.checkVersion: imgui version or data layout mismatch"
  where
    sizeOfC :: Storable a => a -> CSize
    sizeOfC = fromIntegral . sizeOf

-- | Get the compiled version string e.g. "1.80 WIP" (essentially the value for
-- @IMGUI_VERSION@ from the compiled version of @imgui.cpp@).
{-# INLINE getVersion #-}
getVersion :: MonadIO m => m Text
getVersion = liftIO do
  ImGui.getVersion >>= Text.peekCString

-- | Enable keyboard navigation
{-# INLINE enableKeyboardNav #-}
enableKeyboardNav :: MonadIO m => m ()
enableKeyboardNav = liftIO $
  modifyConfigFlags (.|. ImGuiConfigFlags.NavEnableKeyboard)

-- | Disable keyboard navigation
{-# INLINE disableKeyboardNav #-}
disableKeyboardNav :: MonadIO m => m ()
disableKeyboardNav = liftIO $
  modifyConfigFlags (.&. complement ImGuiConfigFlags.NavEnableKeyboard)

-- | Check if keyboard navigation is enabled
{-# INLINE isKeyboardNavEnabled #-}
isKeyboardNavEnabled :: MonadIO m => m Bool
isKeyboardNavEnabled = liftIO do
  flags <- peekIO (.configFlags)
  pure $ (flags .&. ImGuiConfigFlags.NavEnableKeyboard) /= 0

peekIO :: Storable a => (Ptr ImGuiIO -> Ptr a) -> IO a
peekIO field = ImGui.getIO >>= peek . field

modifyConfigFlags :: (ImGuiConfigFlags -> ImGuiConfigFlags) -> IO ()
modifyConfigFlags f = do
  io <- ImGui.getIO
  flags <- peek io.configFlags
  poke io.configFlags (f flags)

-- | Create demo window. Demonstrate most ImGui features. Call this to learn
-- about the library! Try to make it always available in your application!
{-# INLINE showDemoWindow #-}
showDemoWindow :: MonadIO m => m ()
showDemoWindow = liftIO $ ImGui.showDemoWindow nullPtr

-- | Create Metrics/Debugger window. Display Dear ImGui internals: windows, draw
-- commands, various internal state, etc.
{-# INLINE showMetricsWindow #-}
showMetricsWindow :: MonadIO m => m ()
showMetricsWindow = liftIO $ ImGui.showMetricsWindow nullPtr

-- | Create Debug Log window. display a simplified log of important dear imgui events.
{-# INLINE showDebugLogWindow #-}
showDebugLogWindow :: MonadIO m => m ()
showDebugLogWindow = liftIO $ ImGui.showDebugLogWindow nullPtr

-- | Create Stack Tool window. Hover items with mouse to query information about
-- the source of their unique ID.
{-# INLINE showIDStackToolWindow #-}
showIDStackToolWindow :: MonadIO m => m ()
showIDStackToolWindow = liftIO $ ImGui.showIDStackToolWindow nullPtr

-- | Create About window. display Dear ImGui version, credits and build/system
-- information.
{-# INLINE showAboutWindow #-}
showAboutWindow :: MonadIO m => m ()
showAboutWindow = liftIO $ ImGui.showAboutWindow nullPtr

-- | Add style selector block (not a window), essentially a combo listing the
-- default styles.
{-# INLINE showStyleSelector #-}
showStyleSelector :: MonadIO m => Text -> m Bool
showStyleSelector label = liftIO $
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.showStyleSelector labelPtr

-- | Add font selector block (not a window), essentially a combo listing the
-- loaded fonts.
{-# INLINE showFontSelector #-}
showFontSelector :: MonadIO m => Text -> m ()
showFontSelector label = liftIO $
  Text.withCString label ImGui.showFontSelector

-- | Add basic help/info block (not a window): how to manipulate ImGui as a
-- end-user (mouse/keyboard controls).
{-# INLINE showUserGuide #-}
showUserGuide :: MonadIO m => m ()
showUserGuide = liftIO ImGui.showUserGuide

-- | Helper to display buttons for logging to tty/file/clipboard.
{-# INLINE logButtons #-}
logButtons :: MonadIO m => m ()
logButtons = liftIO ImGui.logButtons

-- | New, recommended style (default).
{-# INLINE styleColorsDark #-}
styleColorsDark :: MonadIO m => m ()
styleColorsDark = liftIO $ ImGui.styleColorsDark nullPtr

-- | Best used with borders and a custom, thicker font.
{-# INLINE styleColorsLight #-}
styleColorsLight :: MonadIO m => m ()
styleColorsLight = liftIO $ ImGui.styleColorsLight nullPtr

-- | Classic ImGui style.
{-# INLINE styleColorsClassic #-}
styleColorsClassic :: MonadIO m => m ()
styleColorsClassic = liftIO $ ImGui.styleColorsClassic nullPtr

-- | Push window to the stack and start appending to it.
--
-- Returns 'False' to indicate the window is collapsed or fully clipped, so you
-- may early out and omit submitting anything to the window. Always call a
-- matching 'end' for each 'begin' call, regardless of its return value!
--
-- Wraps @ImGui::Begin()@ with default options.
{-# INLINE begin #-}
begin :: MonadIO m => Text -> m Bool
begin name = liftIO do
  Text.withCString name \namePtr ->
    toBool <$> ImGui.begin namePtr nullPtr 0

-- | Begin a window with a close button.
--
-- Returns (whether window is visible, whether window is still open).
-- The close button appears in the upper-right corner.
--
-- Always call 'end' regardless of return values.
{-# INLINE beginWithClose #-}
beginWithClose :: MonadIO m => Text -> m (Bool, Bool)
beginWithClose name = liftIO $
  with (fromBool True) \openPtr ->
    Text.withCString name \namePtr -> do
      visible <- toBool <$> ImGui.begin namePtr openPtr 0
      stillOpen <- toBool <$> peek openPtr
      pure (visible, stillOpen)

-- | Pop window from the stack.
--
-- Wraps @ImGui::End()@.
{-# INLINE end #-}
end :: MonadIO m => m ()
end = liftIO ImGui.end

-- | Append items to a window.
--
-- Action will get 'False' if the window is collapsed or fully clipped.
--
-- You may append multiple times to the same window during the same frame
-- by calling 'withWindow' in multiple places.
{-# INLINE withWindow #-}
withWindow :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withWindow name = bracket (begin name) (const end)

-- | Append items to a window unless it is collapsed or fully clipped.
--
-- You may append multiple times to the same window during the same frame
-- by calling 'withWindowOpen' in multiple places.
{-# INLINE withWindowOpen #-}
withWindowOpen :: MonadUnliftIO m => Text -> m () -> m ()
withWindowOpen name action =
  withWindow name (`when` action)

-- | Append items to a closeable window unless it is collapsed or fully clipped.
--
-- You may append multiple times to the same window during the same frame
-- by calling 'withWindowOpen' in multiple places.
--
-- The 'Bool' state variable will be set to 'False' when the window's close
-- button is pressed.
{-# INLINE withCloseableWindow #-}
withCloseableWindow :: (HasSetter ref Bool, MonadUnliftIO m) => Text -> ref -> m () -> m ()
withCloseableWindow name ref action = bracket open (const end) (`when` action)
  where
    open = do
      (isVisible, isOpen) <- beginWithClose name
      unless isOpen $ ref $=! False
      pure isVisible

-- | Append items to a fullscreen window.
--
-- The action runs inside a window that is set to behave as a backdrop.
-- It has no typical window decorations, ignores events and does not jump to front.
--
-- You may append multiple times to it during the same frame
-- by calling 'withFullscreen' in multiple places.
{-# INLINE withFullscreen #-}
withFullscreen :: MonadUnliftIO m => m () -> m ()
withFullscreen action = bracket open (const end) (`when` action)
  where
    open = liftIO do
      setNextWindowFullscreen
      Text.withCString "FullScreen" \namePtr ->
        toBool <$> ImGui.begin namePtr nullPtr fullscreenFlags

fullscreenFlags :: ImGuiWindowFlags
fullscreenFlags = foldl' (.|.) zeroBits
  [ ImGuiWindowFlags.NoBackground
  , ImGuiWindowFlags.NoBringToFrontOnFocus
  , ImGuiWindowFlags.NoDecoration
  , ImGuiWindowFlags.NoFocusOnAppearing
  , ImGuiWindowFlags.NoMove
  , ImGuiWindowFlags.NoResize
  , ImGuiWindowFlags.NoSavedSettings
  , ImGuiWindowFlags.NoScrollbar
  , ImGuiWindowFlags.NoScrollWithMouse
  , ImGuiWindowFlags.NoTitleBar
  ]

-- | Get draw list associated to the current window, to append your own drawing primitives
{-# INLINE getWindowDrawList #-}
getWindowDrawList :: MonadIO m => m DrawList
getWindowDrawList = liftIO ImGui.getWindowDrawList

-- | Get current window position in screen space.
--
-- Useful if you want to do your own drawing via the "DrawList" API.
{-# INLINE getWindowPos #-}
getWindowPos :: MonadIO m => m ImVec2
getWindowPos = liftIO ImGui.getWindowPos

{-# INLINE getWindowSize #-}
getWindowSize :: MonadIO m => m ImVec2
getWindowSize = liftIO ImGui.getWindowSize

{-# INLINE getWindowWidth #-}
getWindowWidth :: MonadIO m => m Float
getWindowWidth = liftIO ImGui.getWindowWidth

{-# INLINE getWindowHeight #-}
getWindowHeight :: MonadIO m => m Float
getWindowHeight = liftIO ImGui.getWindowHeight

-- | Check if window is current window appearing
{-# INLINE isWindowAppearing #-}
isWindowAppearing :: MonadIO m => m Bool
isWindowAppearing = liftIO $ toBool <$> ImGui.isWindowAppearing

-- | Check if window is current window collapsed
{-# INLINE isWindowCollapsed #-}
isWindowCollapsed :: MonadIO m => m Bool
isWindowCollapsed = liftIO $ toBool <$> ImGui.isWindowCollapsed

-- | Check if window is current window focused
{-# INLINE isWindowFocused #-}
isWindowFocused :: MonadIO m => ImGuiFocusedFlags -> m Bool
isWindowFocused flags = liftIO $ toBool <$> ImGui.isWindowFocused flags

-- | Set next window position. Call before `begin` Use pivot=(0.5,0.5) to center on given point, etc.
--
-- Wraps @ImGui::SetNextWindowPos()@
{-# INLINE setNextWindowPos #-}
setNextWindowPos
  :: (MonadIO m, HasGetter ref ImVec2)
  => ref
  -> ImGuiCond
  -> Maybe ref -- XXX: the type should be distinct, but using `setNextWindowPos .. Nothing` is ambiguous resulting in bad UX.
  -> m ()
setNextWindowPos posRef cond pivotMaybe = liftIO do
  pos <- get posRef
  pivot <- maybe (pure (ImVec2 0 0)) get pivotMaybe
  ImGui.setNextWindowPos pos cond pivot

-- | Set next window size. Call before `begin`
--
-- Wraps @ImGui::SetNextWindowSize()@
{-# INLINE setNextWindowSize #-}
setNextWindowSize :: (MonadIO m, HasGetter ref ImVec2) => ref -> ImGuiCond -> m ()
setNextWindowSize sizeRef cond = liftIO do
  size' <- get sizeRef
  ImGui.setNextWindowSize size' cond

-- | Set next window to cover the whole display.
{-# INLINE setNextWindowFullscreen #-}
setNextWindowFullscreen :: MonadIO m => m ()
setNextWindowFullscreen = liftIO do
  displaySize <- peekIO (.displaySize)
  ImGui.setNextWindowPos (ImVec2 0 0) 0 (ImVec2 0 0)
  ImGui.setNextWindowSize displaySize 0

-- | Set next window content size (~ scrollable client area, which enforce the range of scrollbars). Not including window decorations (title bar, menu bar, etc.) nor WindowPadding. call before `begin`
--
-- Wraps @ImGui::SetNextWindowContentSize()@
{-# INLINE setNextWindowContentSize #-}
setNextWindowContentSize :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
setNextWindowContentSize sizeRef = liftIO do
  size' <- get sizeRef
  ImGui.setNextWindowContentSize size'

-- | Set next window size limits. use -1,-1 on either X/Y axis to preserve the current size. Sizes will be rounded down.
--
-- Wraps @ImGui::SetNextWindowContentSize()@
{-# INLINE setNextWindowSizeConstraints #-}
setNextWindowSizeConstraints :: (MonadIO m, HasGetter ref ImVec2) => ref -> ref -> m ()
setNextWindowSizeConstraints sizeMinRef sizeMaxRef = liftIO do
  sizeMin <- get sizeMinRef
  sizeMax <- get sizeMaxRef
  ImGui.setNextWindowSizeConstraints sizeMin sizeMax nullFunPtr nullPtr

-- | Set next window collapsed state. call before `begin`
--
-- Wraps @ImGui::SetNextWindowCollapsed()@
{-# INLINE setNextWindowCollapsed #-}
setNextWindowCollapsed :: MonadIO m => Bool -> ImGuiCond -> m ()
setNextWindowCollapsed b cond = liftIO do
  ImGui.setNextWindowCollapsed (fromBool b) cond

-- | Set next window to be focused / top-most. call before `begin`
{-# INLINE setNextWindowFocus #-}
setNextWindowFocus :: MonadIO m => m ()
setNextWindowFocus = liftIO ImGui.setNextWindowFocus

{-# INLINE setNextWindowScroll #-}
setNextWindowScroll :: MonadIO m => ImVec2 -> m ()
setNextWindowScroll = liftIO . ImGui.setNextWindowScroll

-- | Set next window background color alpha. helper to easily override the Alpha component of @ImGuiCol.WindowBg@, @ChildBg@, @PopupBg@. you may also use @ImGuiWindowFlags.NoBackground@.
--
-- Wraps @ImGui::SetNextWindowBgAlpha()@
{-# INLINE setNextWindowBgAlpha #-}
setNextWindowBgAlpha :: MonadIO m => Float -> m ()
setNextWindowBgAlpha = liftIO . ImGui.setNextWindowBgAlpha

-- | Retrieve available space from a given point.
--
-- @== GetContentRegionMax() - GetCursorPos()@
{-# INLINE getContentRegionAvail #-}
getContentRegionAvail :: MonadIO m => m ImVec2
getContentRegionAvail = liftIO ImGui.getContentRegionAvail

-- | Current content boundaries (typically window boundaries including scrolling, or current column boundaries), in windows coordinates.
--
-- @== GetContentRegionAvail() + GetCursorScreenPos() - GetWindowPos()@
{-# INLINE getContentRegionMax #-}
getContentRegionMax :: MonadIO m => m ImVec2
getContentRegionMax = liftIO do
  ImVec2 availX availY <- ImGui.getContentRegionAvail
  ImVec2 cursorX cursorY <- ImGui.getCursorScreenPos
  ImVec2 windowX windowY <- ImGui.getWindowPos
  pure $ ImVec2 (availX + cursorX - windowX) (availY + cursorY - windowY)

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
{-# INLINE beginChild #-}
beginChild :: MonadIO m => Text -> ImVec2 -> Bool -> ImGuiWindowFlags -> m Bool
beginChild name size border flags = liftIO do
  Text.withCString name \namePtr ->
    toBool <$> ImGui.beginChild namePtr size (bool 0 ImGuiChildFlags.Borders border) flags

-- | Wraps @ImGui::EndChild()@.
{-# INLINE endChild #-}
endChild :: MonadIO m => m ()
endChild = liftIO ImGui.endChild

-- | Action wrapper for child windows.
--
-- Action will get 'False' if the child region is collapsed or fully clipped.
{-# INLINE withChild #-}
withChild :: MonadUnliftIO m => Text -> ImVec2 -> Bool -> ImGuiWindowFlags -> (Bool -> m a) -> m a
withChild name size border flags = bracket (beginChild name size border flags) (const endChild)

-- | Action-skipping wrapper for child windows.
--
-- Action will be skipped if the child region is collapsed or fully clipped.
{-# INLINE withChildOpen #-}
withChildOpen :: MonadUnliftIO m => Text -> ImVec2 -> Bool -> ImGuiWindowFlags -> m () -> m ()
withChildOpen name size border flags action =
  withChild name size border flags (`when` action)

-- | Action wrapper to run in a context of another child window addressed by its name.
--
-- Action will get 'False' if the child region is collapsed or fully clipped.
{-# INLINE withChildContext #-}
withChildContext :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withChildContext name action =
  bracket
    (liftIO $ Text.withCString name \namePtr -> toBool <$> ImGui.beginChild namePtr (ImVec2 0 0) 0 0)
    (const endChild)
    action

-- | Plain text.
{-# INLINE text #-}
text :: MonadIO m => Text -> m ()
text t = liftIO do
  Text.withCStringEnd t ImGui.textUnformatted

-- | Colored text.
{-# INLINE textColored #-}
textColored :: (HasGetter ref ImVec4, MonadIO m) => ref -> Text -> m ()
textColored ref t = liftIO do
  color <- get ref
  Text.withCString t (ImGui.textColoredUnformatted color)

-- | Plain text in a "disabled" color according to current style.
{-# INLINE textDisabled #-}
textDisabled :: MonadIO m => Text -> m ()
textDisabled t = liftIO do
  Text.withCString t ImGui.textDisabledUnformatted

-- | Plain text with a word-wrap capability.
--
-- Note that this won't work on an auto-resizing window if there's no other widgets to extend the window width,
-- you may need to set a size using 'setNextWindowSize'.
{-# INLINE textWrapped #-}
textWrapped :: MonadIO m => Text -> m ()
textWrapped t = liftIO do
  Text.withCString t ImGui.textWrappedUnformatted

-- | Label+text combo aligned to other label+value widgets.
{-# INLINE labelText #-}
labelText :: MonadIO m => Text -> Text -> m ()
labelText label t = liftIO do
  Text.withCString label \labelPtr ->
    Text.withCString t (ImGui.labelTextUnformatted labelPtr)

-- | Text with a little bullet aligned to the typical tree node.
{-# INLINE bulletText #-}
bulletText :: MonadIO m => Text -> m ()
bulletText t = liftIO do
  Text.withCString t ImGui.bulletTextUnformatted

-- | Text with an horizontal line.
{-# INLINE separatorText #-}
separatorText :: MonadIO m => Text -> m ()
separatorText t = liftIO do
  Text.withCString t ImGui.separatorText

-- | Shortcut for a labelled Bool.
{-# INLINE valueBool #-}
valueBool :: MonadIO m => Text -> Bool -> m ()
valueBool t b =
  text $ t <> ": " <> bool "false" "true" b

-- | Shortcut for a labelled Int.
{-# INLINE valueInt32 #-}
valueInt32 :: MonadIO m => Text -> Int32 -> m ()
valueInt32 = valueShow

-- | Shortcut for a labelled Word.
{-# INLINE valueWord32 #-}
valueWord32 :: MonadIO m => Text -> Word32 -> m ()
valueWord32 = valueShow

valueShow :: (MonadIO m, Show a) => Text -> a -> m ()
valueShow t v =
  text $ t <> ": " <> Text.pack (show v)

-- | Shortcut for a labelled Float, rendered with a @printf@-style format.
{-# INLINE valueFloat #-}
valueFloat :: MonadIO m => Text -> Float -> Text -> m ()
valueFloat t f format =
  text $ t <> ": " <> Text.pack (printf (Text.unpack format) f)

-- | A button. Returns 'True' when clicked.
--
-- Wraps @ImGui::Button()@.
{-# INLINE button #-}
button :: MonadIO m => Text -> m Bool
button label = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.button labelPtr (ImVec2 0 0)

-- | Button with @FramePadding=(0,0)@ to easily embed within text.
--
-- Wraps @ImGui::SmallButton()@.
{-# INLINE smallButton #-}
smallButton :: MonadIO m => Text -> m Bool
smallButton label = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.smallButton labelPtr

-- | Flexible button behavior without the visuals.
--
-- Frequently useful to build custom behaviors using the public api
-- (along with IsItemActive, IsItemHovered, etc).
--
-- Wraps @ImGui::InvisibleButton()@.
{-# INLINE invisibleButton #-}
invisibleButton :: MonadIO m => Text -> ImVec2 -> ImGuiButtonFlags -> m Bool
invisibleButton label size flags = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.invisibleButton labelPtr size flags

-- | Square button with an arrow shape.
--
-- Wraps @ImGui::ArrowButton()@.
{-# INLINE arrowButton #-}
arrowButton :: MonadIO m => Text -> ImGuiDir -> m Bool
arrowButton strId dir = liftIO do
  Text.withCString strId \strIdPtr ->
    toBool <$> ImGui.arrowButton strIdPtr dir

-- | Draw a texture.
--
-- Wraps @ImGui::Image()@.
{-# INLINE image #-}
image :: MonadIO m => ImTextureRef -> ImVec2 -> ImVec2 -> ImVec2 -> m ()
image texRef size uv0 uv1 = liftIO $ ImGui.image texRef size uv0 uv1

-- | Draw a texture with background and tint colors.
--
-- Wraps @ImGui::ImageWithBg()@.
{-# INLINE imageWithBg #-}
imageWithBg :: MonadIO m => ImTextureRef -> ImVec2 -> ImVec2 -> ImVec2 -> ImVec4 -> ImVec4 -> m ()
imageWithBg texRef size uv0 uv1 bgCol tintCol = liftIO $
  ImGui.imageWithBg texRef size uv0 uv1 bgCol tintCol

-- | Wraps @ImGui::ImageButton()@.
{-# INLINE imageButton #-}
imageButton :: MonadIO m => Text -> ImTextureRef -> ImVec2 -> ImVec2 -> ImVec2 -> ImVec4 -> ImVec4 -> m Bool
imageButton strId texRef size uv0 uv1 bgCol tintCol = liftIO $
  Text.withCString strId \strIdPtr ->
    toBool <$> ImGui.imageButton strIdPtr texRef size uv0 uv1 bgCol tintCol

-- | Wraps @ImGui::Checkbox()@.
{-# INLINE checkbox #-}
checkbox :: (HasSetter ref Bool, HasGetter ref Bool, MonadIO m) => Text -> ref -> m Bool
checkbox label ref = liftIO do
  currentValue <- get ref
  with (fromBool currentValue) \boolPtr -> do
    changed <- Text.withCString label \labelPtr ->
      toBool <$> ImGui.checkbox labelPtr boolPtr

    when changed do
      newValue <- peek boolPtr
      ref $=! (newValue == 1)

    return changed

-- | Checkbox for a bit mask inside a signed value.
{-# INLINE checkboxFlags #-}
checkboxFlags :: (HasSetter ref Int32, HasGetter ref Int32, MonadIO m) => Text -> ref -> Int32 -> m Bool
checkboxFlags label ref flagsValue = liftIO do
  currentValue <- get ref
  Text.withCString label \labelPtr ->
    with (CInt currentValue) \flagsPtr -> do
      changed <- toBool <$> ImGui.checkboxFlagsIntPtr labelPtr flagsPtr (CInt flagsValue)

      when changed do
        CInt newValue <- peek flagsPtr
        ref $=! newValue

      return changed

-- | Checkbox for a bit mask inside an unsigned value.
{-# INLINE checkboxFlagsU #-}
checkboxFlagsU :: (HasSetter ref Word32, HasGetter ref Word32, MonadIO m) => Text -> ref -> Word32 -> m Bool
checkboxFlagsU label ref flagsValue = liftIO do
  currentValue <- get ref
  Text.withCString label \labelPtr ->
    with (CUInt currentValue) \flagsPtr -> do
      changed <- toBool <$> ImGui.checkboxFlagsUintPtr labelPtr flagsPtr (CUInt flagsValue)

      when changed do
        CUInt newValue <- peek flagsPtr
        ref $=! newValue

      return changed

{-# INLINE radioButton #-}
radioButton :: MonadIO m => Text -> Bool -> m Bool
radioButton label b = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.radioButton labelPtr (fromBool b)

{-# INLINE radioButtonI #-}
radioButtonI :: (HasSetter ref Int32, HasGetter ref Int32, MonadIO m) => Text -> ref -> Int32 -> m Bool
radioButtonI label ref vButton = liftIO do
  currentValue <- get ref
  Text.withCString label \labelPtr ->
    with (CInt currentValue) \valuePtr -> do
      changed <- toBool <$> ImGui.radioButtonIntPtr labelPtr valuePtr (CInt vButton)

      when changed do
        CInt newValue <- peek valuePtr
        ref $=! newValue

      return changed

{-# INLINE progressBar #-}
progressBar :: MonadIO m => Float -> Maybe Text -> m ()
progressBar progress overlay = liftIO do
  Text.withCStringOrNull overlay \overlayPtr ->
    ImGui.progressBar progress (ImVec2 negativeFloatMin 0) overlayPtr
  where
    negativeFloatMin = -1.17549435e-38

-- | Draw a small circle + keep the cursor on the same line.
--
-- Advance cursor x position by @GetTreeNodeToLabelSpacing()@,
-- same distance that TreeNode() uses.
{-# INLINE bullet #-}
bullet :: MonadIO m => m ()
bullet = liftIO ImGui.bullet

-- | Begin creating a combo box with a given label and preview value.
--
-- Returns 'True' if the combo box is open. In this state, you should populate
-- the contents of the combo box - for example, by calling 'selectable'.
--
-- Only call 'endCombo' if 'beginCombo' returns 'True'!
--
-- Wraps @ImGui::BeginCombo()@.
{-# INLINE beginCombo #-}
beginCombo :: MonadIO m => Text -> Text -> m Bool
beginCombo label previewValue = liftIO $
  Text.withCString label        \labelPtr ->
  Text.withCString previewValue \previewValuePtr ->
  toBool <$> ImGui.beginCombo labelPtr previewValuePtr 0

-- | Only call 'endCombo' if 'beginCombo' returns 'True'!
--
-- Wraps @ImGui::EndCombo()@.
{-# INLINE endCombo #-}
endCombo :: MonadIO m => m ()
endCombo = liftIO ImGui.endCombo

-- | Create a combo box with a given label and preview value.
--
-- Action will get 'True' if the combo box is open.
-- In this state, you should populate the contents of the combo box - for example, by calling 'selectable'.
{-# INLINE withCombo #-}
withCombo :: MonadUnliftIO m => Text -> Text -> (Bool -> m a) -> m a
withCombo label previewValue =
  bracket (beginCombo label previewValue) (`when` endCombo)

-- | Create a combo box with a given label and preview value.
--
-- Action will be called if the combo box is open to populate the contents
-- of the combo box - for example, by calling 'selectable'.
{-# INLINE withComboOpen #-}
withComboOpen :: MonadUnliftIO m => Text -> Text -> m () -> m ()
withComboOpen label previewValue action =
  withCombo label previewValue (`when` action)

-- | Wraps @ImGui::Combo()@.
{-# INLINE combo #-}
combo :: (MonadIO m, HasGetter ref Int, HasSetter ref Int) => Text -> ref -> [Text] -> m Bool
combo label selectedIndex items = liftIO $ Managed.with m return
  where
    m = do
      i <- get selectedIndex

      cStrings <- traverse (\str -> Managed.managed (Text.withCString str)) items
      labelPtr <- Managed.managed $ Text.withCString label
      iPtr     <- Managed.managed $ with (fromIntegral i)

      liftIO $ withArrayLen cStrings \len itemsPtr -> do
        changed <- toBool <$> ImGui.comboChar labelPtr iPtr itemsPtr (fromIntegral len) (-1)

        when changed do
          i' <- peek iPtr
          selectedIndex $=! fromIntegral i'

        return changed

-- | Wraps @ImGui::DragFloat()@
{-# INLINE dragFloat #-}
dragFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat desc ref speed minValue maxValue = liftIO do
  currentValue <- get ref
  with currentValue \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.dragFloat descPtr floatPtr speed minValue maxValue nullPtr 0

    when changed do
      newValue <- peek floatPtr
      ref $=! newValue

    return changed

-- | Wraps @ImGui::DragFloat2()@
{-# INLINE dragFloat2 #-}
dragFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat2 desc ref speed minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ x, y ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.dragFloat2 descPtr floatPtr speed minValue maxValue nullPtr 0

    when changed do
      [x', y'] <- peekArray 2 floatPtr
      ref $=! (x', y')

    return changed

-- | Wraps @ImGui::DragFloat3()@
{-# INLINE dragFloat3 #-}
dragFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat3 desc ref speed minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ x, y, z ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.dragFloat3 descPtr floatPtr speed minValue maxValue nullPtr 0

    when changed do
      [x', y', z'] <- peekArray 3 floatPtr
      ref $=! (x', y', z')

    return changed

-- | Wraps @ImGui::DragFloat4()@
{-# INLINE dragFloat4 #-}
dragFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat4 desc ref speed minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ x, y, z, u ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.dragFloat4 descPtr floatPtr speed minValue maxValue nullPtr 0

    when changed do
      [x', y', z', u'] <- peekArray 4 floatPtr
      ref $=! (x', y', z', u')

    return changed

{-# INLINE dragFloatRange2 #-}
dragFloatRange2 :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> ref -> Float -> Float -> Float -> Text -> Text -> m Bool
dragFloatRange2 desc refMin refMax speed minValue maxValue minFmt maxFmt = liftIO do
  curMin <- get refMin
  curMax <- get refMax
  with curMin \minPtr ->
    with curMax \maxPtr -> do
      changed <-
        Text.withCString desc \descPtr ->
          Text.withCString minFmt \minFmtPtr ->
            Text.withCString maxFmt \maxFmtPtr ->
              toBool <$> ImGui.dragFloatRange2
                descPtr
                minPtr maxPtr
                speed minValue maxValue
                minFmtPtr maxFmtPtr
                ImGuiSliderFlags.AlwaysClamp

      when changed do
        nextMin <- peek minPtr
        nextMax <- peek maxPtr
        refMin $=! nextMin
        refMax $=! nextMax

      return changed

-- | Wraps @ImGui::DragFloat()@
{-# INLINE dragInt #-}
dragInt :: (MonadIO m, HasSetter ref Int, HasGetter ref Int) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt label ref speed minValue maxValue = liftIO do
  currentValue <- get ref
  with (fromIntegral currentValue) \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.dragInt
          labelPtr
          vPtr
          speed
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      newValue <- peek vPtr
      ref $=! fromIntegral newValue

    return changed

-- | Wraps @ImGui::DragInt2()@
{-# INLINE dragInt2 #-}
dragInt2 :: (MonadIO m, HasSetter ref (Int, Int), HasGetter ref (Int, Int)) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt2 label ref speed minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ fromIntegral x, fromIntegral y ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.dragInt2
          labelPtr
          vPtr
          speed
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      [x', y'] <- peekArray 2 vPtr
      ref $=! (fromIntegral x', fromIntegral y')

    return changed

-- | Wraps @ImGui::DragInt3()@
{-# INLINE dragInt3 #-}
dragInt3 :: (MonadIO m, HasSetter ref (Int, Int, Int), HasGetter ref (Int, Int, Int)) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt3 label ref speed minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.dragInt3
          labelPtr
          vPtr
          speed
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      [x', y', z'] <- peekArray 3 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z')

    return changed

-- | Wraps @ImGui::DragInt4()@
{-# INLINE dragInt4 #-}
dragInt4 :: (MonadIO m, HasSetter ref (Int, Int, Int, Int), HasGetter ref (Int, Int, Int, Int)) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt4 label ref speed minValue maxValue = liftIO do
  (x, y, z, w) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z, fromIntegral w ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.dragInt4
          labelPtr
          vPtr
          speed
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      [x', y', z', w'] <- peekArray 4 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z', fromIntegral w')

    return changed

{-# INLINE dragIntRange2 #-}
dragIntRange2 :: (MonadIO m, HasSetter ref Int, HasGetter ref Int) => Text -> ref -> ref -> Float -> Int -> Int -> Text -> Text -> m Bool
dragIntRange2 desc refMin refMax speed minValue maxValue minFmt maxFmt = liftIO do
  curMin <- get refMin
  curMax <- get refMax
  with (fromIntegral curMin) \minPtr ->
    with (fromIntegral curMax) \maxPtr -> do
      changed <-
        Text.withCString desc \descPtr ->
          Text.withCString minFmt \minFmtPtr ->
            Text.withCString maxFmt \maxFmtPtr ->
              toBool <$> ImGui.dragIntRange2
                descPtr
                minPtr
                maxPtr
                speed
                (fromIntegral minValue)
                (fromIntegral maxValue)
                minFmtPtr maxFmtPtr
                ImGuiSliderFlags.AlwaysClamp

      when changed do
        nextMin <- peek minPtr
        nextMax <- peek maxPtr
        refMin $=! fromIntegral nextMin
        refMax $=! fromIntegral nextMax

      return changed

{-# INLINE dragScalar #-}
dragScalar
  :: (HasSetter ref a, HasGetter ref a, HasGetter range a, Storable a, MonadIO m)
  => Text -> ImGuiDataType -> ref -> Float -> range -> range -> Text -> ImGuiSliderFlags -> m Bool
dragScalar label dataType ref vSpeed refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with currentValue \dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          Text.withCString label \labelPtr ->
            Text.withCString format \formatPtr ->
              toBool <$> ImGui.dragScalar
                labelPtr
                dataType
                (castPtr dataPtr)
                vSpeed
                (castPtr minPtr)
                (castPtr maxPtr)
                formatPtr
                flags

        when changed do
          newValue <- peek dataPtr
          ref $=! newValue

        return changed

{-# INLINE dragScalarN #-}
dragScalarN
  :: (HasSetter ref [a], HasGetter ref [a], HasGetter range a, Storable a, MonadIO m)
  => Text -> ImGuiDataType -> ref -> Float -> range -> range -> Text -> ImGuiSliderFlags -> m Bool
dragScalarN label dataType ref vSpeed refMin refMax format flags = liftIO do
  currentValues <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  withArrayLen currentValues \components dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          Text.withCString label \labelPtr ->
            Text.withCString format \formatPtr ->
              toBool <$> ImGui.dragScalarN
                labelPtr
                dataType
                (castPtr dataPtr)
                (fromIntegral components)
                vSpeed
                (castPtr minPtr)
                (castPtr maxPtr)
                formatPtr
                flags

        when changed do
          newValue <- peekArray components dataPtr
          ref $=! newValue

        return changed

{-# INLINE sliderScalar #-}
sliderScalar
  :: (HasGetter ref a, HasSetter ref a, HasGetter range a, Storable a, MonadIO m)
  => Text -> ImGuiDataType -> ref -> range -> range -> Text -> ImGuiSliderFlags -> m Bool
sliderScalar label dataType ref refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with currentValue \dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          Text.withCString label \labelPtr ->
            Text.withCString format \formatPtr ->
              toBool <$> ImGui.sliderScalar
                labelPtr
                dataType
                (castPtr dataPtr)
                (castPtr minPtr)
                (castPtr maxPtr)
                formatPtr
                flags

        when changed do
          newValue <- peek dataPtr
          ref $=! newValue

        return changed

{-# INLINE sliderScalarN #-}
sliderScalarN
  :: (HasSetter value [a], HasGetter value [a], HasGetter range a, Storable a, MonadIO m)
  => Text -> ImGuiDataType -> value -> range -> range -> Text -> ImGuiSliderFlags -> m Bool
sliderScalarN label dataType ref refMin refMax format flags = liftIO do
  currentValues <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  withArrayLen currentValues \components dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          Text.withCString label \labelPtr ->
            Text.withCString format \formatPtr ->
              toBool <$> ImGui.sliderScalarN
                labelPtr
                dataType
                (castPtr dataPtr)
                (fromIntegral components)
                (castPtr minPtr)
                (castPtr maxPtr)
                formatPtr
                flags

        when changed do
          newValue <- peekArray components dataPtr
          ref $=! newValue

        return changed

-- | Wraps @ImGui::SliderFloat()@
{-# INLINE sliderFloat #-}
sliderFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> m Bool
sliderFloat desc ref minValue maxValue = liftIO do
  currentValue <- get ref
  with currentValue \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.sliderFloat descPtr floatPtr minValue maxValue nullPtr 0

    when changed do
      newValue <- peek floatPtr
      ref $=! newValue

    return changed

-- | Wraps @ImGui::SliderFloat2()@
{-# INLINE sliderFloat2 #-}
sliderFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => Text -> ref -> Float -> Float -> m Bool
sliderFloat2 desc ref minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ x, y ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.sliderFloat2 descPtr floatPtr minValue maxValue nullPtr 0

    when changed do
      [x', y'] <- peekArray 2 floatPtr
      ref $=! (x', y')

    return changed

-- | Wraps @ImGui::SliderFloat3()@
{-# INLINE sliderFloat3 #-}
sliderFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => Text -> ref -> Float -> Float -> m Bool
sliderFloat3 desc ref minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ x, y, z ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.sliderFloat3 descPtr floatPtr minValue maxValue nullPtr 0

    when changed do
      [x', y', z'] <- peekArray 3 floatPtr
      ref $=! (x', y', z')

    return changed

-- | Wraps @ImGui::SliderFloat4()@
{-# INLINE sliderFloat4 #-}
sliderFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => Text -> ref -> Float -> Float -> m Bool
sliderFloat4 desc ref minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ x, y, z, u ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.sliderFloat4 descPtr floatPtr minValue maxValue nullPtr 0

    when changed do
      [x', y', z', u'] <- peekArray 4 floatPtr
      ref $=! (x', y', z', u')

    return changed

-- | Slider widget to select an angle in radians, while displaying degrees.
{-# INLINE sliderAngle #-}
sliderAngle :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> m Bool
sliderAngle desc refRads minDegs maxDegs = liftIO do
  currentRads <- get refRads
  with currentRads \currentRadsPtr -> do
    changed <-
      Text.withCString desc \descPtr ->
        Text.withCString "%.0f deg" \formatPtr ->
          toBool <$> ImGui.sliderAngle descPtr currentRadsPtr minDegs maxDegs formatPtr ImGuiSliderFlags.AlwaysClamp

    when changed do
      newRads <- peek currentRadsPtr
      refRads $=! newRads

    return changed

-- | Wraps @ImGui::SliderInt()@
{-# INLINE sliderInt #-}
sliderInt
  :: (MonadIO m, HasSetter ref Int, HasGetter ref Int)
  => Text -> ref -> Int -> Int -> m Bool
sliderInt label ref minValue maxValue = liftIO do
  currentValue <- get ref
  with (fromIntegral currentValue) \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.sliderInt
          labelPtr
          vPtr
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      newValue <- peek vPtr
      ref $=! fromIntegral newValue

    return changed

-- | Wraps @ImGui::SliderInt2()@
{-# INLINE sliderInt2 #-}
sliderInt2
  :: (MonadIO m, HasSetter ref (Int, Int), HasGetter ref (Int, Int))
  => Text -> ref -> Int -> Int -> m Bool
sliderInt2 label ref minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ fromIntegral x, fromIntegral y ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.sliderInt2
          labelPtr
          vPtr
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      [x', y'] <- peekArray 2 vPtr
      ref $=! (fromIntegral x', fromIntegral y')

    return changed

-- | Wraps @ImGui::SliderInt3()@
{-# INLINE sliderInt3 #-}
sliderInt3
  :: (MonadIO m, HasSetter ref (Int, Int, Int), HasGetter ref (Int, Int, Int))
  => Text -> ref -> Int -> Int -> m Bool
sliderInt3 label ref minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.sliderInt3
          labelPtr
          vPtr
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      [x', y', z'] <- peekArray 3 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z')

    return changed

-- | Wraps @ImGui::SliderInt4()@
{-# INLINE sliderInt4 #-}
sliderInt4
  :: (MonadIO m, HasSetter ref (Int, Int, Int, Int), HasGetter ref (Int, Int, Int, Int))
  => Text -> ref -> Int -> Int -> m Bool
sliderInt4 label ref minValue maxValue = liftIO do
  (x, y, z, w) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z, fromIntegral w] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.sliderInt4
          labelPtr
          vPtr
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      [x', y', z', w'] <- peekArray 4 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z', fromIntegral w')

    return changed

{-# INLINE vSliderFloat #-}
vSliderFloat
  :: (HasSetter ref Float, HasGetter ref Float, MonadIO m)
  => Text -> ImVec2 -> ref -> Float -> Float -> m Bool
vSliderFloat label size ref minValue maxValue = liftIO do
  currentValue <- get ref

  with currentValue \dataPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.vSliderFloat
          labelPtr
          size
          dataPtr
          minValue
          maxValue
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      newValue <- peek dataPtr
      ref $=! newValue

    return changed

{-# INLINE vSliderInt #-}
vSliderInt
  :: (HasSetter ref Int, HasGetter ref Int, MonadIO m)
  => Text -> ImVec2 -> ref -> Int -> Int -> m Bool
vSliderInt label size ref minValue maxValue = liftIO do
  currentValue <- get ref

  with (fromIntegral currentValue) \dataPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        toBool <$> ImGui.vSliderInt
          labelPtr
          size
          dataPtr
          (fromIntegral minValue)
          (fromIntegral maxValue)
          nullPtr
          ImGuiSliderFlags.AlwaysClamp

    when changed do
      newValue <- peek dataPtr
      ref $=! fromIntegral newValue

    return changed

{-# INLINE vSliderScalar #-}
vSliderScalar
  :: (HasSetter ref a, HasGetter ref a, HasGetter range a, Storable a, MonadIO m)
  => Text -> ImVec2 -> ImGuiDataType -> ref -> range -> range -> Text -> ImGuiSliderFlags -> m Bool
vSliderScalar label size dataType ref refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with currentValue \dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          Text.withCString label \labelPtr ->
            Text.withCString format \formatPtr ->
              toBool <$> ImGui.vSliderScalar
                labelPtr
                size
                dataType
                (castPtr dataPtr)
                (castPtr minPtr)
                (castPtr maxPtr)
                formatPtr
                flags

        when changed do
          newValue <- peek dataPtr
          ref $=! newValue

        return changed

-- | Wraps @ImGui::InputText()@.
{-# INLINE inputText #-}
inputText :: (MonadIO m, HasSetter ref Text, HasGetter ref Text) => Text -> ref -> Int -> m Bool
inputText label ref bufSize =
  withInputString ref bufSize \(bufPtr, bufLen) ->
    Text.withCString label \labelPtr ->
      toBool <$> ImGui.inputText
        labelPtr
        bufPtr
        (fromIntegral bufLen)
        0
        nullFunPtr
        nullPtr

-- | Wraps @ImGui::InputTextMultiline()@.
{-# INLINE inputTextMultiline #-}
inputTextMultiline :: (MonadIO m, HasSetter ref Text, HasGetter ref Text) => Text -> ref -> Int -> ImVec2 -> m Bool
inputTextMultiline label ref bufSize size =
  withInputString ref bufSize \(bufPtr, bufLen) ->
    Text.withCString label \labelPtr ->
      toBool <$> ImGui.inputTextMultiline
        labelPtr
        bufPtr
        (fromIntegral bufLen)
        size
        0
        nullFunPtr
        nullPtr

-- | Wraps @ImGui::InputTextWithHint()@.
{-# INLINE inputTextWithHint #-}
inputTextWithHint :: (MonadIO m, HasSetter ref Text, HasGetter ref Text) => Text -> Text -> ref -> Int -> m Bool
inputTextWithHint label hint ref bufSize =
  withInputString ref bufSize \(bufPtr, bufLen) ->
    Text.withCString label \labelPtr ->
      Text.withCString hint \hintPtr ->
        toBool <$> ImGui.inputTextWithHint
          labelPtr
          hintPtr
          bufPtr
          (fromIntegral bufLen)
          0
          nullFunPtr
          nullPtr

-- | Wraps @ImGui::InputText()@ and sets the @ImGuiInputTextFlags.Password@ flag.
{-# INLINE inputPassword #-}
inputPassword :: (MonadIO m, HasSetter ref Text, HasGetter ref Text) => Text -> ref -> Int -> m Bool
inputPassword label ref bufSize =
  withInputString ref bufSize \(bufPtr, bufLen) ->
    Text.withCString label \labelPtr ->
      toBool <$> ImGui.inputText
        labelPtr
        bufPtr
        (fromIntegral bufLen)
        ImGuiInputTextFlags.Password
        nullFunPtr
        nullPtr

-- | Internal helper to prepare appropriately sized and encoded input buffer.
{-# INLINE withInputString #-}
withInputString
  :: (MonadIO m, HasSetter ref Text, HasGetter ref Text)
  => ref
  -> Int
  -> (CStringLen -> IO Bool)
  -> m Bool
withInputString ref bufSize action = liftIO do
  input <- get ref
  Text.withCStringLen input \(refPtr, refSize) ->
    -- XXX: Allocate and zero buffer to receive imgui updates.
    bracket (mkBuf refSize) free \bufPtr -> do
      -- XXX: Copy the original input.
      copyBytes bufPtr refPtr refSize

      changed <- action (bufPtr, max bufSize (refSize + 1))

      when changed do
        -- XXX: Assuming Imgui wouldn't write over the bump stop so peekCString would finish.
        newValue <- Text.peekCString bufPtr
        ref $=! newValue

      return changed
  where
    mkBuf refSize =
      callocBytes $
        max refSize bufSize +
        5 -- XXX: max size of UTF8 code point + NUL terminator

{-# INLINE inputFloat #-}
inputFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> m Bool
inputFloat desc ref step stepFast = liftIO do
  currentValue <- get ref
  with currentValue \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputFloat descPtr floatPtr step stepFast nullPtr 0

    when changed do
      newValue <- peek floatPtr
      ref $=! newValue

    return changed

{-# INLINE inputFloat2 #-}
inputFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => Text -> ref -> m Bool
inputFloat2 desc ref = liftIO do
  (x, y) <- get ref
  withArray [ x, y ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputFloat2 descPtr floatPtr nullPtr 0

    when changed do
      [x', y'] <- peekArray 2 floatPtr
      ref $=! (x', y')

    return changed

{-# INLINE inputFloat3 #-}
inputFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => Text -> ref -> m Bool
inputFloat3 desc ref = liftIO do
  (x, y, z) <- get ref
  withArray [ x, y, z ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputFloat3 descPtr floatPtr nullPtr 0

    when changed do
      [x', y', z'] <- peekArray 3 floatPtr
      ref $=! (x', y', z')

    return changed

{-# INLINE inputFloat4 #-}
inputFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => Text -> ref -> m Bool
inputFloat4 desc ref = liftIO do
  (x, y, z, u) <- get ref
  withArray [ x, y, z, u ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputFloat4 descPtr floatPtr nullPtr 0

    when changed do
      [x', y', z', u'] <- peekArray 4 floatPtr
      ref $=! (x', y', z', u')

    return changed

{-# INLINE inputInt #-}
inputInt :: (MonadIO m, HasSetter ref Int32, HasGetter ref Int32) => Text -> ref -> Int32 -> Int32 -> m Bool
inputInt desc ref step stepFast = liftIO do
  currentValue <- get ref
  with (CInt currentValue) \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputInt descPtr intPtr (CInt step) (CInt stepFast) 0

    when changed do
      CInt newValue <- peek intPtr
      ref $=! newValue

    return changed

{-# INLINE inputInt2 #-}
inputInt2 :: (MonadIO m, HasSetter ref (Int32, Int32), HasGetter ref (Int32, Int32)) => Text -> ref -> m Bool
inputInt2 desc ref = liftIO do
  (x, y) <- get ref
  withArray [ CInt x, CInt y ] \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputInt2 descPtr intPtr 0

    when changed do
      [CInt x', CInt y'] <- peekArray 2 intPtr
      ref $=! (x', y')

    return changed

{-# INLINE inputInt3 #-}
inputInt3 :: (MonadIO m, HasSetter ref (Int32, Int32, Int32), HasGetter ref (Int32, Int32, Int32)) => Text -> ref -> m Bool
inputInt3 desc ref = liftIO do
  (x, y, z) <- get ref
  withArray [ CInt x, CInt y, CInt z ] \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputInt3 descPtr intPtr 0

    when changed do
      [CInt x', CInt y', CInt z'] <- peekArray 3 intPtr
      ref $=! (x', y', z')

    return changed

{-# INLINE inputInt4 #-}
inputInt4 :: (MonadIO m, HasSetter ref (Int32, Int32, Int32, Int32), HasGetter ref (Int32, Int32, Int32, Int32)) => Text -> ref -> m Bool
inputInt4 desc ref = liftIO do
  (x, y, z, u) <- get ref
  withArray [ CInt x, CInt y, CInt z, CInt u ] \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.inputInt4 descPtr intPtr 0

    when changed do
      [CInt x', CInt y', CInt z', CInt u'] <- peekArray 4 intPtr
      ref $=! (x', y', z', u')

    return changed

{-# INLINE inputScalar #-}
inputScalar
  :: (HasGetter ref a, HasSetter ref a, HasGetter range a, Storable a, MonadIO m)
  => Text -> ImGuiDataType -> ref -> range -> range -> Text -> ImGuiInputTextFlags -> m Bool
inputScalar label dataType ref refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with currentValue \dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          Text.withCString label \labelPtr ->
            Text.withCString format \formatPtr ->
              toBool <$> ImGui.inputScalar
                labelPtr
                dataType
                (castPtr dataPtr)
                (castPtr minPtr)
                (castPtr maxPtr)
                formatPtr
                flags

        when changed do
          newValue <- peek dataPtr
          ref $=! newValue

        return changed

{-# INLINE inputScalarN #-}
inputScalarN
  :: (HasSetter value [a], HasGetter value [a], HasGetter range a, Storable a, MonadIO m)
  => Text -> ImGuiDataType -> value -> range -> range -> Text -> ImGuiInputTextFlags -> m Bool
inputScalarN label dataType ref refMin refMax format flags = liftIO do
  currentValues <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  withArrayLen currentValues \components dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          Text.withCString label \labelPtr ->
            Text.withCString format \formatPtr ->
              toBool <$> ImGui.inputScalarN
                labelPtr
                dataType
                (castPtr dataPtr)
                (fromIntegral components)
                (castPtr minPtr)
                (castPtr maxPtr)
                formatPtr
                flags

        when changed do
          newValue <- peekArray components dataPtr
          ref $=! newValue

        return changed

-- | Wraps @ImGui::ColorEdit3()@.
{-# INLINE colorEdit3 #-}
colorEdit3 :: (MonadIO m, HasSetter ref ImVec3, HasGetter ref ImVec3) => Text -> ref -> m Bool
colorEdit3 desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.colorEdit3 descPtr (castPtr refPtr) 0

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed

-- | Wraps @ImGui::ColorEdit4()@.
{-# INLINE colorEdit4 #-}
colorEdit4 :: (MonadIO m, HasSetter ref ImVec4, HasGetter ref ImVec4) => Text -> ref -> m Bool
colorEdit4 desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.colorEdit4 descPtr (castPtr refPtr) 0

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed

-- | Wraps @ImGui::ColorPicker3()@.
{-# INLINE colorPicker3 #-}
colorPicker3 :: (MonadIO m, HasSetter ref ImVec3, HasGetter ref ImVec3) => Text -> ref -> m Bool
colorPicker3 desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      toBool <$> ImGui.colorPicker3 descPtr (castPtr refPtr) 0

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed

-- | Wraps @ImGui::ColorPicker4()@.
{-# INLINE colorPicker4 #-}
colorPicker4 :: (MonadIO m, HasSetter ref ImVec4, HasGetter ref ImVec4) => Text -> ref -> Maybe ImVec4 -> m Bool
colorPicker4 desc ref refColor = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      maybeWith with refColor \refColorPtr ->
        toBool <$> ImGui.colorPicker4 descPtr (castPtr refPtr) 0 (castPtr refColorPtr)

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed

-- | Display a color square/button, hover for details, return true when pressed.
--
-- Wraps @ImGui::ColorButton()@.
{-# INLINE colorButton #-}
colorButton :: (MonadIO m, HasGetter ref ImVec4) => Text -> ref -> m Bool
colorButton desc ref = liftIO do
  currentValue <- get ref
  Text.withCString desc \descPtr ->
    toBool <$> ImGui.colorButton descPtr currentValue 0 (ImVec2 0 0)

data TableOptions = TableOptions
  { tableFlags      :: ImGuiTableFlags
  , tableOuterSize  :: ImVec2
  , tableInnerWidth :: Float
  } deriving Show

defTableOptions :: TableOptions
defTableOptions = TableOptions
  { tableFlags      = 0
  , tableOuterSize  = ImVec2 0  0
  , tableInnerWidth = 0
  }

-- | Wraps @ImGui::BeginTable()@.
{-# INLINE beginTable #-}
beginTable :: MonadIO m => TableOptions -> Text -> Int -> m Bool
beginTable TableOptions{..} label columns = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.beginTable labelPtr (fromIntegral columns) tableFlags tableOuterSize tableInnerWidth

-- | Only call 'endTable' if 'beginTable' returns true!
--
-- Wraps @ImGui::EndTable()@.
{-# INLINE endTable #-}
endTable :: MonadIO m => m ()
endTable = liftIO ImGui.endTable

-- | Create a table.
--
-- The action will get 'False' if the entry is not visible.
--
-- ==== __Example usage:__
--
-- > withTableOpen defTableOptions "MyTable" do
-- >   tableSetupColumn "Hello"
-- >   tableSetupColumn "World"
-- >   tableHeadersRow
-- >
-- >   for_ [("a","1"),("b","2")] \(a,b) -> do
-- >     tableNextRow
-- >     tableNextColumn (text a)
-- >     tableNextColumn (text b)
--
-- Displays:
--
-- @
-- | Hello | World |
-- +-------+-------+
-- | a     | 1     |
-- | b     | 2     |
-- @
--
{-# INLINE withTable #-}
withTable :: MonadUnliftIO m => TableOptions -> Text -> Int -> (Bool -> m a) -> m a
withTable options label columns =
  bracket (beginTable options label columns) (`when` endTable)

{-# INLINE withTableOpen #-}
withTableOpen :: MonadUnliftIO m => TableOptions -> Text -> Int -> m () -> m ()
withTableOpen options label columns action =
  withTable options label columns (`when` action)

-- | Wraps @ImGui::TableNextRow()@ with 'defTableRowOptions'.
--   append into the first cell of a new row.
{-# INLINE tableNextRow #-}
tableNextRow :: MonadIO m => m ()
tableNextRow = tableNextRowWith defTableRowOptions

data TableRowOptions = TableRowOptions
  { tableRowFlags     :: ImGuiTableRowFlags
  , tableRowMinHeight :: Float
  } deriving Show

defTableRowOptions :: TableRowOptions
defTableRowOptions = TableRowOptions
  { tableRowFlags     = 0
  , tableRowMinHeight = 0
  }

-- | Wraps @ImGui::TableNextRow()@ with explicit options.
{-# INLINE tableNextRowWith #-}
tableNextRowWith :: MonadIO m => TableRowOptions -> m ()
tableNextRowWith TableRowOptions{..} = liftIO do
  ImGui.tableNextRow tableRowFlags tableRowMinHeight

{-# INLINE tableNextColumn #-}
tableNextColumn :: MonadIO m => m () -> m ()
tableNextColumn action = liftIO (toBool <$> ImGui.tableNextColumn) >>= (`when` action)

-- | Wraps @ImGui::TableSetColumnIndex()@.
--   append into the specified column. Return true when column is visible.
{-# INLINE tableSetColumnIndex #-}
tableSetColumnIndex :: MonadIO m => Int -> m Bool
tableSetColumnIndex column = liftIO do
  toBool <$> ImGui.tableSetColumnIndex (fromIntegral column)

data TableColumnOptions = TableColumnOptions
  { tableColumnFlags             :: ImGuiTableColumnFlags
  , tableColumnInitWidthOrWeight :: Float
  , tableColumnUserId            :: ImGuiID
  } deriving Show

defTableColumnOptions :: TableColumnOptions
defTableColumnOptions = TableColumnOptions
  { tableColumnFlags             = 0
  , tableColumnInitWidthOrWeight = 0
  , tableColumnUserId            = 0
  }

-- | Wraps @ImGui::TableSetupColumn()@ using 'defTableColumnOptions'.
{-# INLINE tableSetupColumn #-}
tableSetupColumn :: MonadIO m => Text -> m ()
tableSetupColumn = tableSetupColumnWith defTableColumnOptions

-- | Wraps @ImGui::TableSetupColumn() with explicit options@.
{-# INLINE tableSetupColumnWith #-}
tableSetupColumnWith :: MonadIO m => TableColumnOptions -> Text -> m ()
tableSetupColumnWith TableColumnOptions{..} label = liftIO do
  Text.withCString label \labelPtr ->
    ImGui.tableSetupColumn labelPtr tableColumnFlags tableColumnInitWidthOrWeight tableColumnUserId

-- | Wraps @ImGui::TableSetupScrollFreeze()@.
--   lock columns/rows so they stay visible when scrolled.
{-# INLINE tableSetupScrollFreeze #-}
tableSetupScrollFreeze :: MonadIO m => Int -> Int -> m ()
tableSetupScrollFreeze cols rows = liftIO do
  ImGui.tableSetupScrollFreeze (fromIntegral cols) (fromIntegral rows)

-- | Wraps @ImGui::TableHeadersRow()@.
--   submit all headers cells based on data provided to 'tableSetupColumn' + submit context menu
{-# INLINE tableHeadersRow #-}
tableHeadersRow :: MonadIO m => m ()
tableHeadersRow = liftIO ImGui.tableHeadersRow

-- | Wraps @ImGui::TableHeader()@.
--   submit one header cell manually (rarely used)
{-# INLINE tableHeader #-}
tableHeader :: MonadIO m => Text -> m ()
tableHeader label = liftIO $
  Text.withCString label ImGui.tableHeader

data TableSortingSpecs = TableSortingSpecs
  { tableSortingColumn  :: Int -- ^ Index of the column, starting at 0
  , tableSortingReverse :: Bool
  , tableSortingUserId  :: ImGuiID -- ^ User id of the column (if specified by a 'tableSetupColumn' call).
  } deriving (Eq, Ord, Show)

convertTableSortingSpecs :: ImGuiTableColumnSortSpecs -> TableSortingSpecs
convertTableSortingSpecs ImGuiTableColumnSortSpecs{..} =
  TableSortingSpecs
    { tableSortingColumn  = fromIntegral columnIndex
    , tableSortingReverse = sortDirection == ImGuiSortDirection.Descending
    , tableSortingUserId  = columnUserID
    }

-- | High-Level sorting. Returns of the underlying data should be sorted
--   and to what specification. Number of Specifications is mostly 0 or 1, but
--   can be more if @ImGuiTableFlags.SortMulti@ is enabled on the table.
--
--   The Bool only fires true for one frame on each sorting event and resets
--   automatically.
--
--   Must be called AFTER all columns are set up with 'tableSetupColumn'
--
--   Hint: Don't forget to set @ImGuiTableFlags.Sortable@ to enable sorting
--   on tables.
--
-- ==== __Example usage:__
--
-- > sortedData <- newIORef [("a","1"), ("b","2")]
-- >
-- > let sortable = defTableOptions { tableFlags = ImGuiTableFlags.Sortable }
-- > withTableOpen sortable "MyTable" 2 $ do
-- >   tableSetupColumn "Hello"
-- >   tableSetupColumn "World"
-- >
-- >   withSortableTable \isDirty sortSpecs -> do
-- >     when isDirty $
-- >       -- XXX: do your sorting & cache it. Dont sort every frame.
-- >       modifyIORef' sortedData . sortBy $
-- >         foldMap columnSorter sortSpecs
-- >
-- >     tableHeadersRow
-- >     for_ sortedData \(a, b) -> do
-- >       tableNextRow
-- >       tableNextColumn $ text a
-- >       tableNextColumn $ text b
{-# INLINE withSortableTable #-}
withSortableTable :: MonadIO m => (Bool -> [TableSortingSpecs] -> m ()) -> m ()
withSortableTable action = do
  specsPtr <- liftIO ImGui.tableGetSortSpecs
  when (specsPtr /= nullPtr) do
    ImGuiTableSortSpecs{..} <- liftIO $ peek specsPtr
    let isDirty = 0 /= specsDirty
    columns <- liftIO $ peekArray (fromIntegral specsCount) specs

    action isDirty (map convertTableSortingSpecs columns)
    when isDirty $
      liftIO $ poke specsPtr.specsDirty 0

-- | Wraps @ImGui::TableGetColumnCount()@.
--   return number of columns (value passed to BeginTable)
{-# INLINE tableGetColumnCount #-}
tableGetColumnCount :: MonadIO m => m Int
tableGetColumnCount = liftIO $
  fromIntegral <$> ImGui.tableGetColumnCount

-- | Wraps @ImGui::TableGetColumnIndex()@.
--   return current column index.
{-# INLINE tableGetColumnIndex #-}
tableGetColumnIndex :: MonadIO m => m Int
tableGetColumnIndex = liftIO $
  fromIntegral <$> ImGui.tableGetColumnIndex

-- | Wraps @ImGui::TableGetRowIndex()@.
--   return current row index
{-# INLINE tableGetRowIndex #-}
tableGetRowIndex :: MonadIO m => m Int
tableGetRowIndex = liftIO $
  fromIntegral <$> ImGui.tableGetRowIndex

-- | Wraps @ImGui::TableGetColumnName
--   returns "" if column didn't have a name declared by TableSetupColumn
--   'Nothing' returns the current column name
{-# INLINE tableGetColumnName #-}
tableGetColumnName :: MonadIO m => Maybe Int -> m Text
tableGetColumnName c = liftIO do
  ImGui.tableGetColumnName (maybe (-1) fromIntegral c) >>= Text.peekCString

-- | Wraps @ImGui::TableGetRowIndex()@.
--    return column flags so you can query their Enabled/Visible/Sorted/Hovered
--    status flags.
--   'Nothing' returns the current column flags
{-# INLINE tableGetColumnFlags #-}
tableGetColumnFlags :: MonadIO m => Maybe Int -> m ImGuiTableColumnFlags
tableGetColumnFlags c = liftIO $
  ImGui.tableGetColumnFlags (maybe (-1) fromIntegral c)

-- | Wraps @ImGui::TableSetColumnEnabled()@.
--   change user accessible enabled/disabled state of a column. Set to false to
--   hide the column. User can use the context menu to change this themselves
--   (right-click in headers, or right-click in columns body with
--   @ImGuiTableFlags.ContextMenuInBody@)
{-# INLINE tableSetColumnEnabled #-}
tableSetColumnEnabled :: MonadIO m => Int -> Bool -> m ()
tableSetColumnEnabled column_n v = liftIO $
  ImGui.tableSetColumnEnabled (fromIntegral column_n) (fromBool v)

-- | Wraps @ImGui::TableSetBgColor()@.
--   change the color of a cell, row, or column.
--   See 'ImGuiTableBgTarget' flags for details.
--   'Nothing' sets the current row/column color
{-# INLINE tableSetBgColor #-}
tableSetBgColor :: MonadIO m => ImGuiTableBgTarget -> ImU32 -> Maybe Int -> m ()
tableSetBgColor target color column_n = liftIO $
  ImGui.tableSetBgColor target color (maybe (-1) fromIntegral column_n)

-- | Wraps @ImGui::TreeNode()@.
{-# INLINE treeNode #-}
treeNode :: MonadIO m => Text -> m Bool
treeNode label = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.treeNode labelPtr

-- | Wraps @ImGui::TreeNodeEx()@.
{-# INLINE treeNodeWith #-}
treeNodeWith :: MonadIO m => Text -> ImGuiTreeNodeFlags -> m Bool
treeNodeWith label flags = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.treeNodeEx labelPtr flags

-- | Wraps @ImGui::TreePush()@.
{-# INLINE treePush #-}
treePush :: MonadIO m => Text -> m ()
treePush label = liftIO do
  Text.withCString label ImGui.treePush

-- | Wraps @ImGui::TreePop()@.
{-# INLINE treePop #-}
treePop :: MonadIO m => m ()
treePop = liftIO ImGui.treePop

{-# INLINE getTreeNodeToLabelSpacing #-}
getTreeNodeToLabelSpacing :: MonadIO m => m Float
getTreeNodeToLabelSpacing = liftIO ImGui.getTreeNodeToLabelSpacing

{-# INLINE collapsingHeader #-}
collapsingHeader :: MonadIO m => Text -> Maybe Bool -> m Bool
collapsingHeader label visible = liftIO do
  Text.withCString label \labelPtr ->
    maybeWith with (fromBool <$> visible) \visiblePtr ->
      toBool <$> ImGui.collapsingHeaderBoolPtr labelPtr visiblePtr 0

-- | Wraps @ImGui::SetNextItemOpen()@.
{-# INLINE setNextItemOpen #-}
setNextItemOpen :: MonadIO m => Bool -> m ()
setNextItemOpen is_open = liftIO $ ImGui.setNextItemOpen (fromBool is_open) 0

-- | Wraps @ImGui::Selectable()@ with default options.
{-# INLINE selectable #-}
selectable :: MonadIO m => Text -> m Bool
selectable = selectableWith defSelectableOptions

data SelectableOptions = SelectableOptions
  { selected :: Bool
  , flags    :: ImGuiSelectableFlags
  , size     :: ImVec2
  } deriving Show

defSelectableOptions :: SelectableOptions
defSelectableOptions = SelectableOptions
  { selected = False
  , flags    = 0
  , size     = ImVec2 0 0
  }

-- | Wraps @ImGui::Selectable()@ with explicit options.
{-# INLINE selectableWith #-}
selectableWith :: MonadIO m => SelectableOptions -> Text -> m Bool
selectableWith (SelectableOptions selected flags size) label = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.selectable labelPtr (fromBool selected) flags size

{-# INLINE listBox #-}
listBox :: (MonadIO m, HasGetter ref Int, HasSetter ref Int) => Text -> ref -> [Text] -> m Bool
listBox label selectedIndex items = liftIO $ Managed.with m return
  where
    m = do
      i <- get selectedIndex

      cStrings <- traverse (\str -> Managed.managed (Text.withCString str)) items
      labelPtr <- Managed.managed $ Text.withCString label
      iPtr     <- Managed.managed $ with (fromIntegral i)

      liftIO $ withArrayLen cStrings \len itemsPtr -> do
        changed <- toBool <$> ImGui.listBox labelPtr iPtr itemsPtr (fromIntegral len) (-1)

        when changed do
          i' <- peek iPtr
          selectedIndex $=! fromIntegral i'

        return changed

-- | Wraps @ImGui::PlotLines()@.
{-# INLINE plotLines #-}
plotLines :: MonadIO m => Text -> [Float] -> m ()
plotLines = plotValues ImGui.plotLines

-- | Wraps @ImGui::PlotHistogram()@.
{-# INLINE plotHistogram #-}
plotHistogram :: MonadIO m => Text -> [Float] -> m ()
plotHistogram = plotValues ImGui.plotHistogram

plotValues
  :: MonadIO m
  => (CString -> Ptr Float -> CInt -> CInt -> CString -> Float -> Float -> ImVec2 -> CInt -> IO ())
  -> Text -> [Float] -> m ()
plotValues plot label values = liftIO $
  withArrayLen values \len valuesPtr ->
    Text.withCString label \labelPtr ->
      plot labelPtr valuesPtr (fromIntegral len) 0 nullPtr floatMax floatMax (ImVec2 0 0) floatStride

floatMax :: Float
floatMax = 3.4028235e38

floatStride :: CInt
floatStride = fromIntegral $ sizeOf (undefined :: Float)

-- | Create a menu bar at the top of the screen and append to it.
--
-- The action will get 'False' if the menu is not visible.
{-# INLINE withMainMenuBar #-}
withMainMenuBar :: MonadUnliftIO m => (Bool -> m a) -> m a
withMainMenuBar = bracket beginMainMenuBar (`when` endMainMenuBar)

-- | Create a menu bar at the top of the screen and append to it.
--
-- The action will be skipped if the menu is not visible.
{-# INLINE withMainMenuBarOpen #-}
withMainMenuBarOpen :: MonadUnliftIO m => m () -> m ()
withMainMenuBarOpen action =
  withMainMenuBar (`when` action)

-- | Create and append to a full screen menu-bar.
--
-- Wraps @ImGui::BeginMainMenuBar()@
{-# INLINE beginMainMenuBar #-}
beginMainMenuBar :: MonadIO m => m Bool
beginMainMenuBar = liftIO $ toBool <$> ImGui.beginMainMenuBar

-- | Only call 'endMainMenuBar' if 'beginMainMenuBar' returns true!
--
-- Wraps @ImGui::EndMainMenuBar()@
{-# INLINE endMainMenuBar #-}
endMainMenuBar :: MonadIO m => m ()
endMainMenuBar = liftIO ImGui.endMainMenuBar

-- | Append items to a window with MenuBar flag.
--
-- The action will get 'False' if the menu is not visible.
{-# INLINE withMenuBar #-}
withMenuBar :: MonadUnliftIO m => (Bool -> m a) -> m a
withMenuBar = bracket beginMenuBar (`when` endMenuBar)

-- | Append items to a window with MenuBar flag.
--
-- The action will be skipped if the menu is not visible.
{-# INLINE withMenuBarOpen #-}
withMenuBarOpen :: MonadUnliftIO m => m () -> m ()
withMenuBarOpen action =
  withMenuBar (`when` action)

-- | Append to menu-bar of current window (requires 'ImGuiWindowFlags.MenuBar' flag set on parent window).
--
-- Wraps @ImGui::BeginMenuBar()@
{-# INLINE beginMenuBar #-}
beginMenuBar :: MonadIO m => m Bool
beginMenuBar = liftIO $ toBool <$> ImGui.beginMenuBar

-- | Only call 'endMenuBar' if 'beginMenuBar' returns true!
--
-- Wraps @ImGui::EndMenuBar()@
{-# INLINE endMenuBar #-}
endMenuBar :: MonadIO m => m ()
endMenuBar = liftIO ImGui.endMenuBar

-- | Create a sub-menu entry.
--
-- Wraps @ImGui::BeginMenu()@.
{-# INLINE beginMenu #-}
beginMenu :: MonadIO m => Text -> m Bool
beginMenu label = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.beginMenu labelPtr 1

-- | Only call 'endMenu' if 'beginMenu' returns true!
--
-- Wraps @ImGui::EndMenu()@
{-# INLINE endMenu #-}
endMenu :: MonadIO m => m ()
endMenu = liftIO ImGui.endMenu

-- | Create a sub-menu entry.
--
-- The action will get 'False' if the entry is not visible.
{-# INLINE withMenu #-}
withMenu :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withMenu label = bracket (beginMenu label) (`when` endMenu)

-- | Create a sub-menu entry.
--
-- The action will be skipped if the entry is not visible.
{-# INLINE withMenuOpen #-}
withMenuOpen :: MonadUnliftIO m => Text -> m () -> m ()
withMenuOpen label action =
  withMenu label (`when` action)

-- | Return true when activated. Shortcuts are displayed for convenience but not
-- processed by ImGui at the moment
--
-- Wraps @ImGui::MenuItem()@
{-# INLINE menuItem #-}
menuItem :: MonadIO m => Text -> m Bool
menuItem label = liftIO do
  Text.withCString label \labelPtr ->
    toBool <$> ImGui.menuItem labelPtr nullPtr 0 1

-- | Create a @TabBar@ and start appending to it.
--
-- Wraps @ImGui::BeginTabBar@.
{-# INLINE beginTabBar #-}
beginTabBar :: MonadIO m => Text -> ImGuiTabBarFlags -> m Bool
beginTabBar tabBarID flags = liftIO do
  Text.withCString tabBarID \ptr ->
    toBool <$> ImGui.beginTabBar ptr flags

-- | Finish appending elements to a tab bar. Only call if 'beginTabBar' returns @True@.
--
-- Wraps @ImGui::EndTabBar@.
{-# INLINE endTabBar #-}
endTabBar :: MonadIO m => m ()
endTabBar = liftIO ImGui.endTabBar

-- | Create a @TabBar@ and start appending to it.
--
-- The action will get 'False' if the Tab bar is not visible.
{-# INLINE withTabBar #-}
withTabBar :: MonadUnliftIO m => Text -> ImGuiTabBarFlags -> (Bool -> m a) -> m a
withTabBar tabBarID flags =
  bracket (beginTabBar tabBarID flags) (`when` endTabBar)

-- | Create a @TabBar@ and start appending to it.
--
-- The action will be skipped if the Tab bar is not visible.
{-# INLINE withTabBarOpen #-}
withTabBarOpen :: MonadUnliftIO m => Text -> ImGuiTabBarFlags -> m () -> m ()
withTabBarOpen tabBarID flags action =
  withTabBar tabBarID flags (`when` action)

-- | Create a new tab. Returns @True@ if the tab is selected.
--
-- Wraps @ImGui::BeginTabItem@.
{-# INLINE beginTabItem #-}
beginTabItem :: (MonadIO m, HasGetter ref Bool, HasSetter ref Bool) => Text -> ref -> ImGuiTabItemFlags -> m Bool
beginTabItem tabName ref flags = liftIO do
  currentValue <- get ref
  with (fromBool currentValue) \refPtr -> do
    open <- Text.withCString tabName \ptrName ->
      toBool <$> ImGui.beginTabItem ptrName refPtr flags

    newValue <- toBool <$> peek refPtr
    when (newValue /= currentValue) do
      ref $=! newValue

    pure open

-- | Finish appending elements to a tab. Only call if 'beginTabItem' returns @True@.
--
-- Wraps @ImGui::EndTabItem@.
{-# INLINE endTabItem #-}
endTabItem :: MonadIO m => m ()
endTabItem = liftIO ImGui.endTabItem

-- | Create a new tab.
--
-- The action will get 'True' if the tab is selected.
{-# INLINE withTabItem #-}
withTabItem :: (MonadUnliftIO m, HasGetter ref Bool, HasSetter ref Bool) => Text -> ref -> ImGuiTabItemFlags -> (Bool -> m a) -> m a
withTabItem tabName ref flags =
  bracket (beginTabItem tabName ref flags) (`when` endTabItem)

-- | Create a new tab.
--
-- The action will be skipped unless the tab is selected.
{-# INLINE withTabItemOpen #-}
withTabItemOpen :: (MonadUnliftIO m, HasGetter ref Bool, HasSetter ref Bool) => Text -> ref -> ImGuiTabItemFlags -> m () -> m ()
withTabItemOpen tabName ref flags action =
  withTabItem tabName ref flags (`when` action)

-- | Create a tab that behaves like a button. Returns @True@ when clicked. Cannot be selected in the tab bar.
--
-- Wraps @ImGui.TabItemButton@.
{-# INLINE tabItemButton #-}
tabItemButton :: MonadIO m => Text -> ImGuiTabItemFlags -> m Bool
tabItemButton tabName flags = liftIO do
  Text.withCString tabName \namePtr ->
    toBool <$> ImGui.tabItemButton namePtr flags

-- | Notify the tab bar (or the docking system) that a tab/window is about to close.
-- Useful to reduce visual flicker on reorderable tab bars.
--
-- __For tab-bar__: call after 'beginTabBar' and before tab submission. Otherwise, call with a window name.
{-# INLINE setTabItemClosed #-}
setTabItemClosed :: MonadIO m => Text -> m ()
setTabItemClosed tabName = liftIO do
  Text.withCString tabName ImGui.setTabItemClosed

-- | Set a text-only tooltip if preceding item was hovered.
{-# INLINE setItemTooltip #-}
setItemTooltip :: MonadIO m => Text -> m ()
setItemTooltip t = liftIO do
  Text.withCString t ImGui.setItemTooltipUnformatted

-- | Begin/append a tooltip window.
--
-- To create full-featured tooltip (with any kind of items).
--
-- Wraps @ImGui::BeginTooltip()@
{-# INLINE beginTooltip #-}
beginTooltip :: MonadIO m => m Bool
beginTooltip = liftIO $ toBool <$> ImGui.beginTooltip

-- | Begin/append a tooltip window if preceding item was hovered.
--
-- Wraps @ImGui::BeginItemTooltip()@
{-# INLINE beginItemTooltip #-}
beginItemTooltip :: MonadIO m => m Bool
beginItemTooltip = liftIO $ toBool <$> ImGui.beginItemTooltip

-- | Only call if 'beginTooltip' or 'beginItemTooltip' returns true!
--
-- Wraps @ImGui::EndTooltip()@
{-# INLINE endTooltip #-}
endTooltip :: MonadIO m => m ()
endTooltip = liftIO ImGui.endTooltip

-- | Create a tooltip if a previous item is hovered.
--
-- Those are windows that follow a mouse and don't take focus away.
-- Can contain any kind of items.
{-# INLINE withItemTooltip #-}
withItemTooltip ::  MonadUnliftIO m => m () -> m ()
withItemTooltip action = bracket beginItemTooltip (`when` endTooltip) (`when` action)

-- | Create a tooltip.
--
-- Those are windows that follow a mouse and don't take focus away.
-- Can contain any kind of items.
{-# INLINE withTooltip #-}
withTooltip ::  MonadUnliftIO m => m () -> m ()
withTooltip action = bracket beginTooltip (`when` endTooltip) (`when` action)

-- | Action wrapper for disabled blocks.
--
-- See 'beginDisabled' and 'endDisabled' for more info.
{-# INLINE withDisabled #-}
withDisabled :: (MonadUnliftIO m, HasGetter ref Bool) => ref -> m a -> m a
withDisabled disabledRef action = do
  disabled <- get disabledRef
  if disabled then bracket_ (beginDisabled True) endDisabled action else action

-- | Begin a block that may be disabled. This disables all user interactions
-- and dims item visuals.
--
-- Always call a matching 'endDisabled' for each 'beginDisabled' call.
{-# INLINE beginDisabled #-}
beginDisabled :: MonadIO m => Bool -> m ()
beginDisabled = liftIO . ImGui.beginDisabled . fromBool

-- | End a block that may be disabled.
{-# INLINE endDisabled #-}
endDisabled :: MonadIO m => m ()
endDisabled = liftIO ImGui.endDisabled

-- | Returns 'True' if the popup is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopup()@
{-# INLINE beginPopup #-}
beginPopup :: MonadIO m => Text -> m Bool
beginPopup popupId = liftIO do
  Text.withCString popupId \idPtr ->
    toBool <$> ImGui.beginPopup idPtr 0

-- | Only call 'endPopup' if 'beginPopup' returns true!
--
-- Wraps @ImGui::EndPopup()@
{-# INLINE endPopup #-}
endPopup :: MonadIO m => m ()
endPopup = liftIO ImGui.endPopup

-- | Append items to a non-modal Popup.
--
-- Non-modal popups can be closed by clicking anywhere outside them,
-- or by pressing ESCAPE.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will get 'True' if the popup is open.
{-# INLINE withPopup #-}
withPopup :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withPopup popupId = bracket (beginPopup popupId) (`when` endPopup)

-- | Append items to a non-modal Popup.
--
-- Non-modal popups can be closed by clicking anywhere outside them,
-- or by pressing ESCAPE.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will be called only if the popup is open.
{-# INLINE withPopupOpen #-}
withPopupOpen :: MonadUnliftIO m => Text -> m () -> m ()
withPopupOpen popupId action =
  withPopup popupId (`when` action)

-- | Returns 'True' if the modal is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopupModal()@
{-# INLINE beginPopupModal #-}
beginPopupModal :: MonadIO m => Text -> m Bool
beginPopupModal popupId = liftIO do
  Text.withCString popupId \idPtr ->
    toBool <$> ImGui.beginPopupModal idPtr nullPtr 0

-- | Append items to a modal Popup.
--
-- Modal popups can be closed only with 'closeCurrentPopup'.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will get 'True' if the popup is open.
{-# INLINE withPopupModal #-}
withPopupModal :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withPopupModal popupId = bracket (beginPopupModal popupId) (`when` endPopup)

-- | Append intems to a modal Popup.
--
-- Modal popups can be closed only with 'closeCurrentPopup'.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will be called only if the popup is open.
{-# INLINE withPopupModalOpen #-}
withPopupModalOpen :: MonadUnliftIO m => Text -> m () -> m ()
withPopupModalOpen popupId action =
  withPopupModal popupId (`when` action)

{-# INLINE beginPopupContextItem #-}
beginPopupContextItem :: MonadIO m => Maybe Text -> ImGuiPopupFlags -> m Bool
beginPopupContextItem itemId flags = liftIO do
  Text.withCStringOrNull itemId \popupIdPtr ->
    toBool <$> ImGui.beginPopupContextItem popupIdPtr flags

{-# INLINE withPopupContextItem #-}
withPopupContextItem :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> (Bool -> m a) -> m a
withPopupContextItem popupId flags = bracket (beginPopupContextItem popupId flags) (`when` endPopup)

{-# INLINE withPopupContextItemOpen #-}
withPopupContextItemOpen :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> m () -> m ()
withPopupContextItemOpen popupId flags action = withPopupContextItem popupId flags (`when` action)

-- | Attach item context popup to right mouse button click on a last item.
{-# INLINE itemContextPopup #-}
itemContextPopup :: MonadUnliftIO m => m () -> m ()
itemContextPopup = withPopupContextItemOpen Nothing ImGuiPopupFlags.MouseButtonRight

{-# INLINE beginPopupContextWindow #-}
beginPopupContextWindow :: MonadIO m => Maybe Text -> ImGuiPopupFlags -> m Bool
beginPopupContextWindow popupId flags = liftIO do
  Text.withCStringOrNull popupId \popupIdPtr ->
    toBool <$> ImGui.beginPopupContextWindow popupIdPtr flags

{-# INLINE withPopupContextWindow #-}
withPopupContextWindow :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> (Bool -> m a) -> m a
withPopupContextWindow popupId flags = bracket (beginPopupContextWindow popupId flags) (`when` endPopup)

{-# INLINE withPopupContextWindowOpen #-}
withPopupContextWindowOpen :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> m () -> m ()
withPopupContextWindowOpen popupId flags action = withPopupContextWindow popupId flags (`when` action)

-- | Attach item context popup to right mouse button click on a current window.
{-# INLINE windowContextPopup #-}
windowContextPopup :: MonadUnliftIO m => m () -> m ()
windowContextPopup = withPopupContextWindowOpen Nothing ImGuiPopupFlags.MouseButtonRight

{-# INLINE beginPopupContextVoid #-}
beginPopupContextVoid :: MonadIO m => Maybe Text -> ImGuiPopupFlags -> m Bool
beginPopupContextVoid popupId flags = liftIO do
  Text.withCStringOrNull popupId \popupIdPtr ->
    toBool <$> ImGui.beginPopupContextVoid popupIdPtr flags

{-# INLINE withPopupContextVoid #-}
withPopupContextVoid :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> (Bool -> m a) -> m a
withPopupContextVoid popupId flags = bracket (beginPopupContextVoid popupId flags) (`when` endPopup)

{-# INLINE withPopupContextVoidOpen #-}
withPopupContextVoidOpen :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> m () -> m ()
withPopupContextVoidOpen popupId flags action = withPopupContextVoid popupId flags (`when` action)

-- | Attach item context popup to right mouse button click outside of any windows.
{-# INLINE voidContextPopup #-}
voidContextPopup :: MonadUnliftIO m => m () -> m ()
voidContextPopup = withPopupContextVoidOpen Nothing ImGuiPopupFlags.MouseButtonRight

-- | Call to mark popup as open (don't call every frame!).
--
-- Wraps @ImGui::OpenPopup()@
{-# INLINE openPopup #-}
openPopup :: MonadIO m => Text -> m ()
openPopup popupId = liftIO do
  Text.withCString popupId \idPtr ->
    void $ ImGui.openPopup idPtr 0

-- | Opens a defined popup (i.e. defined with 'withPopup') on defined action.
--
-- Example:
--
-- > openPopupOnItemClick "myPopup" ImGuiPopupFlags.MouseButtonRight
--
-- Wraps @ImGui::OpenPopup()@
{-# INLINE openPopupOnItemClick #-}
openPopupOnItemClick :: MonadIO m => Text -> ImGuiPopupFlags -> m ()
openPopupOnItemClick popupId flags = liftIO do
  Text.withCString popupId $ \idPtr ->
    void $ ImGui.openPopupOnItemClick idPtr flags

-- | Manually close the popup we have begin-ed into.
--
-- Wraps @ImGui::CloseCurrentPopup()@
{-# INLINE closeCurrentPopup #-}
closeCurrentPopup :: MonadIO m => m ()
closeCurrentPopup = liftIO ImGui.closeCurrentPopup

-- | Check if the popup is open at the current 'beginPopup' level of the popup stack.
{-# INLINE isCurrentPopupOpen #-}
isCurrentPopupOpen :: MonadIO m => Text -> m Bool
isCurrentPopupOpen popupId = liftIO do
  Text.withCString popupId $ \idPtr ->
    toBool <$> ImGui.isPopupOpen idPtr 0

-- | Check if *any* popup is open at the current 'beginPopup' level of the popup stack.
{-# INLINE isAnyPopupOpen #-}
isAnyPopupOpen :: MonadIO m => Text -> m Bool
isAnyPopupOpen popupId = liftIO do
  Text.withCString popupId $ \idPtr ->
    toBool <$> ImGui.isPopupOpen idPtr ImGuiPopupFlags.AnyPopupId

-- | Check if *any* popup is open at any level of the popup stack.
{-# INLINE isAnyLevelPopupOpen #-}
isAnyLevelPopupOpen :: MonadIO m => Text -> m Bool
isAnyLevelPopupOpen popupId = liftIO do
  Text.withCString popupId $ \idPtr ->
    toBool <$> ImGui.isPopupOpen idPtr (ImGuiPopupFlags.AnyPopupId .|. ImGuiPopupFlags.AnyPopupLevel)

-- | Is the last item hovered? (and usable, aka not blocked by a popup, etc.).
{-# INLINE isItemHovered #-}
isItemHovered :: MonadIO m => m Bool
isItemHovered = liftIO $ toBool <$> ImGui.isItemHovered 0

-- | Is the last item active? (e.g. button being held, text field being edited.
-- This will continuously return true while holding mouse button on an item.
-- Items that don't interact will always return false)
{-# INLINE isItemActive #-}
isItemActive :: MonadIO m => m Bool
isItemActive = liftIO $ toBool <$> ImGui.isItemActive

-- | Is the last item focused for keyboard/gamepad navigation?
{-# INLINE isItemFocused #-}
isItemFocused :: MonadIO m => m Bool
isItemFocused = liftIO $ toBool <$> ImGui.isItemFocused

-- | Is the last item hovered and mouse clicked on?
{-# INLINE isItemClicked #-}
isItemClicked :: MonadIO m => ImGuiMouseButton -> m Bool
isItemClicked button_ = liftIO $ toBool <$> ImGui.isItemClicked button_

-- | Is the last item visible? (items may be out of sight because of clipping/scrolling)
{-# INLINE isItemVisible #-}
isItemVisible :: MonadIO m => m Bool
isItemVisible = liftIO $ toBool <$> ImGui.isItemVisible

-- | Did the last item modify its underlying value this frame? or was pressed? This is generally the same as the "bool" return value of many widgets.
{-# INLINE isItemEdited #-}
isItemEdited :: MonadIO m => m Bool
isItemEdited = liftIO $ toBool <$> ImGui.isItemEdited

-- | Was the last item just made active (item was previously inactive).
{-# INLINE isItemActivated #-}
isItemActivated :: MonadIO m => m Bool
isItemActivated = liftIO $ toBool <$> ImGui.isItemActivated

-- | Was the last item just made inactive (item was previously active). Useful for Undo/Redo patterns with widgets that require continuous editing.
{-# INLINE isItemDeactivated #-}
isItemDeactivated :: MonadIO m => m Bool
isItemDeactivated = liftIO $ toBool <$> ImGui.isItemDeactivated

-- | Was the last item just made inactive and made a value change when it was active? (e.g. Slider/Drag moved). Useful for Undo/Redo patterns with widgets that require continuous editing. Note that you may get false positives (some widgets such as Combo()/ListBox()/Selectable() will return true even when clicking an already selected item).
{-# INLINE isItemDeactivatedAfterEdit #-}
isItemDeactivatedAfterEdit :: MonadIO m => m Bool
isItemDeactivatedAfterEdit = liftIO $ toBool <$> ImGui.isItemDeactivatedAfterEdit

-- | Was the last item open state toggled? set by TreeNode().
{-# INLINE isItemToggledOpen #-}
isItemToggledOpen :: MonadIO m => m Bool
isItemToggledOpen = liftIO $ toBool <$> ImGui.isItemToggledOpen

-- | Is any item hovered?
{-# INLINE isAnyItemHovered #-}
isAnyItemHovered :: MonadIO m => m Bool
isAnyItemHovered = liftIO $ toBool <$> ImGui.isAnyItemHovered

-- | Is any item active?
{-# INLINE isAnyItemActive #-}
isAnyItemActive :: MonadIO m => m Bool
isAnyItemActive = liftIO $ toBool <$> ImGui.isAnyItemActive

-- | Is any item focused?
{-# INLINE isAnyItemFocused #-}
isAnyItemFocused :: MonadIO m => m Bool
isAnyItemFocused = liftIO $ toBool <$> ImGui.isAnyItemFocused

-- | Get ID of last item (~~ often same ImGui::GetID(label) beforehand)
{-# INLINE getItemID #-}
getItemID :: MonadIO m => m ImGuiID
getItemID = liftIO ImGui.getItemID

-- | Get upper-left bounding rectangle of the last item (screen space)
{-# INLINE getItemRectMin #-}
getItemRectMin :: MonadIO m => m ImVec2
getItemRectMin = liftIO ImGui.getItemRectMin

-- | Get lower-right bounding rectangle of the last item (screen space)
{-# INLINE getItemRectMax #-}
getItemRectMax :: MonadIO m => m ImVec2
getItemRectMax = liftIO ImGui.getItemRectMax

-- | Get size of last item
{-# INLINE getItemRectSize #-}
getItemRectSize :: MonadIO m => m ImVec2
getItemRectSize = liftIO ImGui.getItemRectSize

-- | Separator, generally horizontal. inside a menu bar or in horizontal layout
-- mode, this becomes a vertical separator.
--
-- Wraps @ImGui::Separator()@
{-# INLINE separator #-}
separator :: MonadIO m => m ()
separator = liftIO ImGui.separator

-- | Call between widgets or groups to layout them horizontally.
--
-- Wraps @ImGui::SameLine@.
{-# INLINE sameLine #-}
sameLine :: MonadIO m => m ()
sameLine = liftIO $ ImGui.sameLine 0 (-1)

-- | Undo a `sameLine` or force a new line when in an horizontal-layout context.
--
-- Wraps @ImGui::NewLine()@
{-# INLINE newLine #-}
newLine :: MonadIO m => m ()
newLine = liftIO ImGui.newLine

-- | Add vertical spacing.
--
-- Wraps @ImGui::Spacing()@
{-# INLINE spacing #-}
spacing :: MonadIO m => m ()
spacing = liftIO ImGui.spacing

-- | Add a dummy item of given size. unlike `invisibleButton`, `dummy` won't take the mouse click or be navigable into.
--
-- Wraps @ImGui::Dummy()@
{-# INLINE dummy #-}
dummy :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
dummy sizeRef = liftIO do
  size' <- get sizeRef
  ImGui.dummy size'

{-# INLINE withIndent #-}
withIndent :: MonadUnliftIO m => Float -> m a -> m a
withIndent width =
  bracket_ (indent width) (unindent width)

-- | Move content position toward the right, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Indent()@
{-# INLINE indent #-}
indent :: MonadIO m => Float -> m ()
indent = liftIO . ImGui.indent

-- | Move content position back to the left, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Unindent()@
{-# INLINE unindent #-}
unindent :: MonadIO m => Float -> m ()
unindent = liftIO . ImGui.unindent

-- | Affect large frame+labels widgets only.
--
-- Wraps @ImGui::SetNextItemWidth()@
{-# INLINE setNextItemWidth #-}
setNextItemWidth :: MonadIO m => Float -> m ()
setNextItemWidth = liftIO . ImGui.setNextItemWidth

{-# INLINE withItemWidth #-}
withItemWidth :: MonadUnliftIO m => Float -> m a -> m a
withItemWidth width =
  bracket_ (pushItemWidth width) popItemWidth

-- | Wraps @ImGui::PushItemWidth()@
{-# INLINE pushItemWidth #-}
pushItemWidth :: MonadIO m => Float -> m ()
pushItemWidth = liftIO . ImGui.pushItemWidth

-- | Wraps @ImGui::PopItemWidth()@
{-# INLINE popItemWidth #-}
popItemWidth :: MonadIO m => m ()
popItemWidth = liftIO ImGui.popItemWidth

-- | Width of item given pushed settings and current cursor position.
-- NOT necessarily the width of last item unlike most 'Item' functions.
--
-- Wraps @ImGui::CalcItemWidth()@
{-# INLINE calcItemWidth #-}
calcItemWidth :: MonadIO m => m Float
calcItemWidth = liftIO ImGui.calcItemWidth

{-# INLINE withTextWrapPos #-}
withTextWrapPos :: MonadUnliftIO m => Float -> m a -> m a
withTextWrapPos width =
  bracket_ (pushTextWrapPos width) popTextWrapPos

-- | Push word-wrapping position for Text commands.
--
-- Negative: no wrapping.
-- Zero: wrap to end of window (or column).
-- Positive: wrap at 'wrap_pos_x' position in window local space.
{-# INLINE pushTextWrapPos #-}
pushTextWrapPos :: MonadIO m => Float -> m ()
pushTextWrapPos = liftIO . ImGui.pushTextWrapPos

-- | Wraps @ImGui::PopTextWrapPos()@
{-# INLINE popTextWrapPos #-}
popTextWrapPos :: MonadIO m => m ()
popTextWrapPos = liftIO ImGui.popTextWrapPos

-- | Lock horizontal starting position
--
-- Wraps @ImGui::BeginGroup()@ and @ImGui::EndGroup()@
{-# INLINE withGroup #-}
withGroup :: MonadUnliftIO m => m a -> m a
withGroup = bracket_ beginGroup endGroup

-- | Lock horizontal starting position
--
-- Wraps @ImGui::BeginGroup()@
{-# INLINE beginGroup #-}
beginGroup :: MonadIO m => m ()
beginGroup = liftIO ImGui.beginGroup

-- | Unlock horizontal starting position + capture the whole group bounding box
-- into one "item" (so you can use `isItemHovered` or layout primitives such as
-- `sameLine` on whole group, etc.)
--
-- Wraps @ImGui::EndGroup()@
{-# INLINE endGroup #-}
endGroup :: MonadIO m => m ()
endGroup = liftIO ImGui.endGroup

-- | Set cursor position in window-local coordinates
--
-- Wraps @ImGui::SetCursorPos()@
{-# INLINE setCursorPos #-}
setCursorPos :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
setCursorPos posRef = liftIO do
  pos <- get posRef
  ImGui.setCursorPos pos

{-# INLINE setCursorPosX #-}
setCursorPosX :: MonadIO m => Float -> m ()
setCursorPosX = liftIO . ImGui.setCursorPosX

{-# INLINE setCursorPosY #-}
setCursorPosY :: MonadIO m => Float -> m ()
setCursorPosY = liftIO . ImGui.setCursorPosY

{-# INLINE setCursorScreenPos #-}
setCursorScreenPos :: MonadIO m => ImVec2 -> m ()
setCursorScreenPos = liftIO . ImGui.setCursorScreenPos

-- | Get cursor position in window-local coordinates.
{-# INLINE getCursorPos #-}
getCursorPos :: MonadIO m => m ImVec2
getCursorPos = liftIO ImGui.getCursorPos

{-# INLINE getCursorPosX #-}
getCursorPosX :: MonadIO m => m Float
getCursorPosX = liftIO ImGui.getCursorPosX

{-# INLINE getCursorPosY #-}
getCursorPosY :: MonadIO m => m Float
getCursorPosY = liftIO ImGui.getCursorPosY

-- | Initial cursor position in window-local coordinates.
{-# INLINE getCursorStartPos #-}
getCursorStartPos :: MonadIO m => m ImVec2
getCursorStartPos = liftIO ImGui.getCursorStartPos

-- | Vertically align upcoming text baseline to FramePadding.y so that it will align properly to regularly framed items (call if you have text on a line before a framed item)
{-# INLINE alignTextToFramePadding #-}
alignTextToFramePadding :: MonadIO m => m ()
alignTextToFramePadding = liftIO ImGui.alignTextToFramePadding

{-# INLINE getTextLineHeight #-}
getTextLineHeight :: MonadIO m => m Float
getTextLineHeight = liftIO ImGui.getTextLineHeight

{-# INLINE getTextLineHeightWithSpacing #-}
getTextLineHeightWithSpacing :: MonadIO m => m Float
getTextLineHeightWithSpacing = liftIO ImGui.getTextLineHeightWithSpacing

{-# INLINE getFrameHeight #-}
getFrameHeight :: MonadIO m => m Float
getFrameHeight = liftIO ImGui.getFrameHeight

{-# INLINE getFrameHeightWithSpacing #-}
getFrameHeightWithSpacing :: MonadIO m => m Float
getFrameHeightWithSpacing = liftIO ImGui.getFrameHeightWithSpacing

-- | Add an element to a ID stack
--
-- Read the FAQ (http://dearimgui.org/faq) for more details
-- about how ID are handled in dear imgui.
--
-- Those questions are answered and impacted by understanding of the ID stack system:
-- * "Q: Why is my widget not reacting when I click on it?"
-- * "Q: How can I have widgets with an empty label?"
-- * "Q: How can I have multiple widgets with the same label?"
--
-- Wraps @ImGui::PushId@ and @ImGui::PopId@
{-# INLINE withID #-}
withID :: (MonadUnliftIO m, ToID id) => id -> m a -> m a
withID i = bracket_ (liftIO $ pushID i) (liftIO ImGui.popID)

-- | A supplementary class to match overloaded functions in C++ the library.
class ToID a where
  pushID :: MonadIO m => a -> m ()

instance ToID CInt where
  pushID = liftIO . ImGui.pushIDInt

instance ToID Int where
  pushID = liftIO . ImGui.pushIDInt . fromIntegral

instance ToID Integer where
  pushID = liftIO . ImGui.pushIDInt . fromInteger

instance {-# OVERLAPPABLE #-} ToID (Ptr a) where
  pushID = liftIO . ImGui.pushIDPtr . castPtr

instance {-# OVERLAPPING #-} ToID (Ptr CChar) where
  pushID = liftIO . ImGui.pushID

instance ToID (Ptr CChar, Int) where
  pushID (strPtr, len) = liftIO $ ImGui.pushIDStr strPtr (strPtr `plusPtr` len)

instance ToID Text where
  pushID t = liftIO $ Text.withCStringEnd t ImGui.pushIDStr

{-# INLINE withStyleColor #-}
withStyleColor :: (MonadUnliftIO m, HasGetter ref ImVec4) => ImGuiCol -> ref -> m a -> m a
withStyleColor color ref =
  bracket_ (pushStyleColor color ref) (popStyleColor 1)

-- | Modify a style color by pushing to the shared stack.
--
-- Always use this if you modify the style after `newFrame`.
--
-- Wraps @ImGui::PushStyleColor()@
{-# INLINE pushStyleColor #-}
pushStyleColor :: (MonadIO m, HasGetter ref ImVec4) => ImGuiCol -> ref -> m ()
pushStyleColor col colorRef = liftIO do
  color <- get colorRef
  ImGui.pushStyleColorImVec4 col color

-- | Remove style color modifications from the shared stack
--
-- Wraps @ImGui::PopStyleColor()@
{-# INLINE popStyleColor #-}
popStyleColor :: MonadIO m => Int -> m ()
popStyleColor = liftIO . ImGui.popStyleColor . fromIntegral

{-# INLINE withStyleVar #-}
withStyleVar :: (MonadUnliftIO m, HasGetter ref ImVec2) => ImGuiStyleVar -> ref -> m a -> m a
withStyleVar style ref =
  bracket_ (pushStyleVar style ref) (popStyleVar 1)

-- | Allow/disable focusing using TAB/Shift-TAB, enabled by default but you can disable it for certain widgets.
{-# INLINE withTabStop #-}
withTabStop :: MonadUnliftIO m => Bool -> m a -> m a
withTabStop enabled =
  bracket_ (pushTabStop enabled) popTabStop

-- | Allow/disable focusing using TAB/Shift-TAB, enabled by default but you can disable it for certain widgets.
{-# INLINE pushTabStop #-}
pushTabStop :: MonadIO m => Bool -> m ()
pushTabStop enabled = liftIO $
  ImGui.pushItemFlag ImGuiItemFlags.NoTabStop (fromBool (not enabled))

-- | Undo a 'pushTabStop'.
{-# INLINE popTabStop #-}
popTabStop :: MonadIO m => m ()
popTabStop = liftIO ImGui.popItemFlag

-- | Modify a style variable by pushing to the shared stack.
--
-- Always use this if you modify the style after `newFrame`.
--
-- Wraps @ImGui::PushStyleVar()@
{-# INLINE pushStyleVar #-}
pushStyleVar :: (MonadIO m, HasGetter ref ImVec2) => ImGuiStyleVar -> ref -> m ()
pushStyleVar style valRef = liftIO do
  val <- get valRef
  ImGui.pushStyleVarImVec2 style val

-- | Remove style variable modifications from the shared stack
--
-- Wraps @ImGui::PopStyleVar()@
{-# INLINE popStyleVar #-}
popStyleVar :: MonadIO m => Int -> m ()
popStyleVar n = liftIO do
  ImGui.popStyleVar (fromIntegral n)

-- | Render widgets inside the block using provided font by keeping the current size.
{-# INLINE withFont #-}
withFont :: MonadUnliftIO m => Font -> m a -> m a
withFont font = bracket_ (pushFont font) popFont

-- | Render widgets inside the block using provided font at an explicit size.
{-# INLINE withFontWithSize #-}
withFontWithSize :: MonadUnliftIO m => Font -> Float -> m a -> m a
withFontWithSize font size = bracket_ (pushFontWithSize font size) popFont

-- | Render widgets inside the block using provided font at the size it was added before.
{-# INLINE withFontLegacySize #-}
withFontLegacySize :: MonadUnliftIO m => Font -> m a -> m a
withFontLegacySize font = bracket_ (pushFontLegacySize font) popFont

-- | Render widgets inside the block by keeping the current font, but setting an explicit size.
{-# INLINE withFontSize #-}
withFontSize :: MonadUnliftIO m => Float -> m a -> m a
withFontSize size = bracket_ (pushFontSize size) popFont

-- | Use provided font, keeping the current size.
--
-- Wraps @ImGui::PushFont(font, 0.0f)@
{-# INLINE pushFont #-}
pushFont :: MonadIO m => Font -> m ()
pushFont font = liftIO $ ImGui.pushFontFloat font 0

-- | Use provided font at an explicit size.
--
-- Wraps @ImGui::PushFont(font, size)@
{-# INLINE pushFontWithSize #-}
pushFontWithSize :: MonadIO m => Font -> Float -> m ()
pushFontWithSize font size = liftIO $ ImGui.pushFontFloat font size

-- | Use provided font at the size it was added with.
{-# INLINE pushFontLegacySize #-}
pushFontLegacySize :: MonadIO m => Font -> m ()
pushFontLegacySize font = liftIO do
  size <- peek font.legacySize
  ImGui.pushFontFloat font size

-- | Keep the current font, but set an explicit size.
--
-- Wraps @ImGui::PushFont(NULL, size)@
{-# INLINE pushFontSize #-}
pushFontSize :: MonadIO m => Float -> m ()
pushFontSize size = liftIO $ ImGui.pushFontFloat nullPtr size

-- | Wraps @ImGui::PopFont()@
{-# INLINE popFont #-}
popFont :: MonadIO m => m ()
popFont = liftIO ImGui.popFont

-- | Attach drag-n-drop source with a payload to a preceding item.
--
-- A valid target should have a matching payload type.
--
-- Data is copied and retained by DearImGui.
-- Action is executed when the payload is accepted.
{-# INLINE withDragDropSource #-}
withDragDropSource :: (MonadUnliftIO m, Storable a) => ImGuiDragDropFlags -> Text -> a -> (Bool -> m ()) -> m ()
withDragDropSource flags payloadType payload action =
  withRunInIO \run ->
    with payload \payloadPtr ->
      run $ withDragDropSourceData flags payloadType (castPtr payloadPtr, Foreign.sizeOf payload) action

-- | Attach drag-n-drop target to a preceding item.
--
-- A valid source should have a matching payload type.
--
-- Data is fetched from DearImGui copy and cleared on delivery.
-- Action is executed when the payload is accepted and not empty.
{-# INLINE withDragDropTarget #-}
withDragDropTarget :: (MonadUnliftIO m, Storable a) => ImGuiDragDropFlags -> Text -> (a -> m ()) -> m ()
withDragDropTarget flags payloadType action =
  withRunInIO \run ->
    withAcceptedPayload flags payloadType \payload -> do
      dataPtr <- peek payload.data_
      Foreign.maybePeek peek (castPtr dataPtr) >>= traverse_ (run . action)

-- | Like 'withDragDropSource', but only set payload type.
{-# INLINE withDragDropSource_ #-}
withDragDropSource_ :: (MonadUnliftIO m) => ImGuiDragDropFlags -> Text -> (Bool -> m ()) -> m ()
withDragDropSource_ flags payloadType action =
  withDragDropSourceData flags payloadType (nullPtr :: Ptr (), 0 :: Int) action

-- | Like 'withDragDropTarget', but only set payload type.
--
-- Payload data is ignored.
{-# INLINE withDragDropTarget_ #-}
withDragDropTarget_ :: (MonadUnliftIO m) => ImGuiDragDropFlags -> Text -> m () -> m ()
withDragDropTarget_ flags payloadType action =
  withRunInIO \run ->
    withAcceptedPayload flags payloadType \_payload -> run action

-- | Like 'withDragDropSource', explicitly setting data ptr and size.
--
-- Suitable for data with dynamic lengths via @withCStringLen@-like functions.
{-# INLINE withDragDropSourceData #-}
withDragDropSourceData :: (MonadUnliftIO m, Integral len) => ImGuiDragDropFlags -> Text -> (Ptr a, len) -> (Bool -> m ()) -> m ()
withDragDropSourceData flags payloadType (dataPtr, dataSize) action =
  withRunInIO \run -> do
    open <- toBool <$> ImGui.beginDragDropSource flags
    when open do
      accepted <-
        Text.withCString payloadType \typePtr ->
          toBool <$> ImGui.setDragDropPayload typePtr (castPtr dataPtr) (fromIntegral dataSize) ImGuiCond.Once
      run $ action accepted
      ImGui.endDragDropSource

-- | Like 'withDragDropTarget', getting raw data ptr and size.
--
-- Check the size, and pointer for NULLs etc.!
{-# INLINE withDragDropTargetData #-}
withDragDropTargetData :: (MonadUnliftIO m, Integral len) => ImGuiDragDropFlags -> Text -> ((Ptr a, len) -> m ()) -> m ()
withDragDropTargetData flags payloadType action =
  withRunInIO \run ->
    withAcceptedPayload flags payloadType \payload -> do
      dataPtr <- peek payload.data_
      dataSize <- peek payload.dataSize
      run $ action (castPtr dataPtr, fromIntegral dataSize)

withAcceptedPayload :: ImGuiDragDropFlags -> Text -> (Ptr ImGuiPayload -> IO ()) -> IO ()
withAcceptedPayload flags payloadType action = do
  open <- toBool <$> ImGui.beginDragDropTarget
  when open do
    Text.withCString payloadType \typePtr -> do
      payload <- ImGui.acceptDragDropPayload typePtr flags
      when (payload /= nullPtr) $
        action payload
    ImGui.endDragDropTarget

-- | Clips a large list of items
--
-- The requirements on @a@ are that they are all of the same height.
{-# INLINE withListClipper #-}
withListClipper :: (ClipItems t a, MonadUnliftIO m) => Maybe Float -> t a -> (a -> m ()) -> m ()
withListClipper itemHeight items action =
  withRunInIO \run ->
    ListClipper.withDefault \clipper -> do
      ListClipper.begin clipper itemCount' itemHeight'
      let
        go = do
          doStep <- toBool <$> ListClipper.step clipper
          when doStep do
            startIndex <- fromIntegral <$> peek clipper.displayStart
            endIndex   <- fromIntegral <$> peek clipper.displayEnd
            run $ stepItems action $
              clipItems startIndex endIndex items
            go
      go
  where
    itemHeight' = fromMaybe (-1.0) itemHeight
    itemCount' = maybe maxBound fromIntegral (itemCount items)

-- | Calculate the size of a text with the current font.
{-# INLINE calcTextSize #-}
calcTextSize :: MonadIO m => Text -> Bool -> Float -> m ImVec2
calcTextSize t hideAfterDoubleHash wrapWidth = liftIO do
  Text.withCStringEnd t \textPtr textEndPtr ->
    ImGui.calcTextSize textPtr textEndPtr (fromBool hideAfterDoubleHash) wrapWidth

-- | Containers usable with 'ListClipper'.
class ClipItems t a where
  itemCount :: t a -> Maybe Int
  clipItems :: Int -> Int -> t a -> t a
  stepItems :: Monad m => (a -> m ()) -> t a -> m ()

-- | Unbounded stream of items.
instance ClipItems [] a where
  itemCount = const Nothing

  clipItems displayStart displayEnd =
    take (displayEnd - displayStart) . drop displayStart

  stepItems = mapM_

instance ClipItems V.Vector a where
  itemCount = Just . V.length

  clipItems displayStart displayEnd =
    V.slice displayStart (displayEnd - displayStart)

  stepItems = V.mapM_

instance Storable a => ClipItems VS.Vector a where
  itemCount = Just . VS.length

  clipItems displayStart displayEnd =
    VS.slice displayStart (displayEnd - displayStart)

  stepItems = VS.mapM_

instance VU.Unbox a => ClipItems VU.Vector a where
  itemCount = Just . VU.length

  clipItems displayStart displayEnd =
    VU.slice displayStart (displayEnd - displayStart)

  stepItems = VU.mapM_

-- | ClipList helper for arbitrary unmaterialized ranges.
data ClipRange a = ClipRange a a
  deriving (Eq, Ord, Show)

instance (Ord a, Enum a, Num a) => ClipItems ClipRange a where
  itemCount (ClipRange _begin end_) =
    Just $ fromEnum end_

  clipItems clipBegin clipEnd (ClipRange oldBegin oldEnd) =
    ClipRange
      (toEnum $ max clipBegin $ fromEnum oldBegin)
      (toEnum $ min clipEnd $ fromEnum oldEnd)

  stepItems action (ClipRange start end_) =
    mapM_ action [start .. end_ - 1]

-- | Does the mouse want to be captured by imgui this frame?
--
-- When true, don't dispatch mouse events to the application.
{-# INLINE wantCaptureMouse #-}
wantCaptureMouse :: MonadIO m => m Bool
wantCaptureMouse = liftIO $
  toBool <$> peekIO (.wantCaptureMouse)

-- | Does the keyboard want to be captured by imgui this frame?
--
-- When true, don't dispatch keyboard events to the application.
{-# INLINE wantCaptureKeyboard #-}
wantCaptureKeyboard :: MonadIO m => m Bool
wantCaptureKeyboard = liftIO $
  toBool <$> peekIO (.wantCaptureKeyboard)

-- | Current mouse position in screen space.
{-# INLINE getMousePos #-}
getMousePos :: MonadIO m => m ImVec2
getMousePos = liftIO ImGui.getMousePos

-- | Retrieve mouse position at the time of opening popup we have 'beginPopup' into.
{-# INLINE getMousePosOnOpeningCurrentPopup #-}
getMousePosOnOpeningCurrentPopup :: MonadIO m => m ImVec2
getMousePosOnOpeningCurrentPopup = liftIO ImGui.getMousePosOnOpeningCurrentPopup

-- | Is mouse dragging? (uses @io.MouseDraggingThreshold@ if @lock_threshold < 0.0f@)
{-# INLINE isMouseDragging #-}
isMouseDragging :: MonadIO m => ImGuiMouseButton -> Float -> m Bool
isMouseDragging button_ threshold = liftIO $
  toBool <$> ImGui.isMouseDragging button_ threshold

-- | Return the delta from the initial clicking position while the mouse button is pressed or was just released.
{-# INLINE getMouseDragDelta #-}
getMouseDragDelta :: MonadIO m => ImGuiMouseButton -> Float -> m ImVec2
getMouseDragDelta button_ threshold = liftIO $
  ImGui.getMouseDragDelta button_ threshold

{-# INLINE resetMouseDragDelta #-}
resetMouseDragDelta :: MonadIO m => ImGuiMouseButton -> m ()
resetMouseDragDelta = liftIO . ImGui.resetMouseDragDelta

-- | Wraps @ImGui::Shortcut()@.
{-# INLINE shortcut #-}
shortcut :: MonadIO m => ImGuiKeyChord -> ImGuiInputFlags -> m Bool
shortcut keyChord flags = liftIO $ toBool <$> ImGui.shortcut keyChord flags

-- | Wraps @ImGui::SetNextItemShortcut()@.
{-# INLINE setNextItemShortcut #-}
setNextItemShortcut :: MonadIO m => ImGuiKeyChord -> ImGuiInputFlags -> m ()
setNextItemShortcut keyChord flags = liftIO $ ImGui.setNextItemShortcut keyChord flags

-- | Make last item the default focused item of a window.
{-# INLINE setItemDefaultFocus #-}
setItemDefaultFocus :: MonadIO m => m ()
setItemDefaultFocus = liftIO ImGui.setItemDefaultFocus

-- | Focus keyboard on the next widget. Use positive 'offset' to access sub components of a multiple component widget. Use -1 to access previous widget.
{-# INLINE setKeyboardFocusHere #-}
setKeyboardFocusHere :: MonadIO m => Int -> m ()
setKeyboardFocusHere = liftIO . ImGui.setKeyboardFocusHere . fromIntegral

-- | Allow next item to be overlapped by a subsequent item.
{-# INLINE setNextItemAllowOverlap #-}
setNextItemAllowOverlap :: MonadIO m => m ()
setNextItemAllowOverlap = liftIO ImGui.setNextItemAllowOverlap

-- | This draw list will be the first rendering one.
--
-- Useful to quickly draw shapes/text behind dear imgui contents.
{-# INLINE getBackgroundDrawList #-}
getBackgroundDrawList :: MonadIO m => m DrawList
getBackgroundDrawList = liftIO $ ImGui.getBackgroundDrawList nullPtr

-- | This draw list will be the last rendered one.
--
-- Useful to quickly draw shapes/text over dear imgui contents.
{-# INLINE getForegroundDrawList #-}
getForegroundDrawList :: MonadIO m => m DrawList
getForegroundDrawList = liftIO $ ImGui.getForegroundDrawList nullPtr

-- | Pack a colour from its components.
--
-- Wraps @IM_COL32()@.
imCol32 :: CUChar -> CUChar -> CUChar -> CUChar -> ImU32
imCol32 r g b a =
  fromIntegral r `shiftL` IM_COL32_R_SHIFT
    .|. fromIntegral g `shiftL` IM_COL32_G_SHIFT
    .|. fromIntegral b `shiftL` IM_COL32_B_SHIFT
    .|. fromIntegral a `shiftL` IM_COL32_A_SHIFT

-- | Estimate of application framerate (rolling average over 60 frames, based on @io.DeltaTime@), in frame per second. Solely for convenience.
{-# INLINE framerate #-}
framerate :: MonadIO m => m Float
framerate = liftIO $ peekIO (.framerate)

-- | Get global imgui time.
--
-- Incremented by io.DeltaTime every frame.
{-# INLINE getTime #-}
getTime :: MonadIO m => m Double
getTime = liftIO ImGui.getTime

-- | Get global imgui frame count.
--
-- Incremented by 1 every frame.
{-# INLINE getFrameCount #-}
getFrameCount :: MonadIO m => m Int
getFrameCount = liftIO $ fromIntegral <$> ImGui.getFrameCount
