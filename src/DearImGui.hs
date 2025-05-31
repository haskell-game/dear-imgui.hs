{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: DearImGui

Main ImGui module, exporting the functions to create a GUI.
-}

module DearImGui
  ( -- * Context Creation and Access
    Raw.Context(..)
  , Raw.createContext
  , Raw.destroyContext
  , Raw.getCurrentContext
  , Raw.setCurrentContext

    -- * Main
  , Raw.newFrame
  , Raw.endFrame
  , Raw.render
  , Raw.DrawData(..)
  , Raw.getDrawData
  , Raw.checkVersion

    -- * Demo, Debug, Information
  , Raw.showDemoWindow
  , Raw.showIDStackToolWindow
  , Raw.showMetricsWindow
  , Raw.showAboutWindow
  , Raw.showStyleSelector
  , Raw.showFontSelector
  , Raw.showUserGuide
  , getVersion

    -- * Logging
  , Raw.showDebugLogWindow
  , Raw.logButtons
  , logText

    -- * Styles
  , Raw.styleColorsDark
  , Raw.styleColorsLight
  , Raw.styleColorsClassic

    -- * Windows
  , withWindow
  , withWindowOpen
  , withCloseableWindow
  , withFullscreen
  , fullscreenFlags

  , begin
  , Raw.end

    -- ** Utilities

  , Raw.getWindowDrawList
  , Raw.getWindowPos
  , Raw.getWindowSize
  , getWindowWidth
  , getWindowHeight
  , Raw.isWindowAppearing
  , Raw.isWindowCollapsed
  , Raw.isWindowFocused

    -- ** Manipulation
  , setNextWindowPos
  , setNextWindowSize
  , Raw.setNextWindowFullscreen
  , setNextWindowContentSize
  , setNextWindowSizeConstraints
  , setNextWindowCollapsed
  , Raw.setNextWindowFocus
  , setNextWindowScroll
  , setNextWindowBgAlpha
  , Raw.getContentRegionAvail
  , Raw.getContentRegionMax
  , Raw.getWindowContentRegionMin
  , Raw.getWindowContentRegionMax

    -- ** Child Windows
  , withChild
  , withChildOpen
  , withChildContext
  , beginChild
  , Raw.endChild

    -- * Parameter stacks
  , withStyleColor
  , pushStyleColor
  , Raw.popStyleColor

  , withStyleVar
  , pushStyleVar
  , popStyleVar
  , withTabStop
  , pushTabStop
  , Raw.popTabStop

  , withFont
  , Raw.Font.pushFont
  , Raw.Font.popFont
  , Raw.Font.Font

    -- * Cursor/Layout
  , Raw.separator
  , Raw.sameLine
  , Raw.newLine
  , Raw.spacing
  , dummy

  , withIndent
  , indent
  , unindent

  , setNextItemWidth
  , withItemWidth
  , pushItemWidth
  , Raw.popItemWidth
  , Raw.calcItemWidth
  , withTextWrapPos
  , pushTextWrapPos
  , Raw.popTextWrapPos

  , withGroup
  , Raw.beginGroup
  , Raw.endGroup

  , setCursorPos
  , setCursorPosX
  , setCursorPosY
  , setCursorScreenPos
  , Raw.getCursorPos
  , getCursorPosX
  , getCursorPosY
  , Raw.getCursorStartPos
  , Raw.alignTextToFramePadding
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
  , Raw.image
  , checkbox
  , checkboxFlags
  , checkboxFlagsU
  , radioButton
  , radioButtonI
  , progressBar
  , Raw.bullet

    -- ** Combo Box
  , withCombo
  , withComboOpen
  , beginCombo
  , Raw.endCombo
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
  , inputScalar
  , inputScalarN

    -- ** Color Editor/Picker
  , colorEdit3
  , colorEdit4
  , colorPicker3
  , colorPicker4
  , colorButton
  , Raw.setColorEditOptions

    -- ** Tables
  , withTable
  , withTableOpen
  , TableOptions(..)
  , defTableOptions
  , beginTable
  , Raw.endTable

    -- *** Setup
  , tableSetupColumn
  , tableSetupColumnWith
  , TableColumnOptions(..)
  , defTableColumnOptions

  , Raw.tableHeadersRow
  , Raw.tableHeader
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
  , Raw.treePop
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
  , Raw.beginMenuBar
  , Raw.endMenuBar

  , withMainMenuBar
  , withMainMenuBarOpen
  , Raw.beginMainMenuBar
  , Raw.endMainMenuBar

  , withMenu
  , withMenuOpen
  , beginMenu
  , Raw.endMenu

  , menuItem

    -- ** Tabs, tab bar
  , withTabBar
  , withTabBarOpen
  , beginTabBar
  , Raw.endTabBar

  , withTabItem
  , withTabItemOpen
  , beginTabItem
  , Raw.endTabItem
  , tabItemButton
  , setTabItemClosed

    -- ** Tooltips
  , setItemTooltip
  , withItemTooltip
  , withTooltip
  , Raw.beginTooltip
  , Raw.endTooltip

    -- ** Disabled blocks
  , withDisabled
  , beginDisabled
  , Raw.endDisabled

    -- * Popups/Modals

    -- ** Generic
  , withPopup
  , withPopupOpen
  , beginPopup
  , Raw.endPopup

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
  , Raw.closeCurrentPopup

    -- ** Queries
  , isCurrentPopupOpen
  , isAnyPopupOpen
  , isAnyLevelPopupOpen

    -- * Item/Widgets Utilities
  , Raw.isItemHovered
  , Raw.isItemActive
  , Raw.isItemFocused
  , Raw.isItemClicked
  , Raw.isItemVisible
  , Raw.isItemEdited
  , Raw.isItemActivated
  , Raw.isItemDeactivated
  , Raw.isItemDeactivatedAfterEdit
  , Raw.isItemToggledOpen
  , Raw.isAnyItemHovered
  , Raw.isAnyItemActive
  , Raw.isAnyItemFocused
  , Raw.getItemID
  , Raw.getItemRectMin
  , Raw.getItemRectMax
  , Raw.getItemRectSize

    -- * Utilities
  , Raw.wantCaptureMouse
  , Raw.getMousePos
  , Raw.getMousePosOnOpeningCurrentPopup
  , Raw.isMouseDragging
  , Raw.getMouseDragDelta
  , Raw.resetMouseDragDelta

  , Raw.wantCaptureKeyboard
  , Raw.shortcut
  , Raw.setNextItemShortcut

  , Raw.setItemDefaultFocus
  , Raw.setKeyboardFocusHere
  , Raw.setNextItemAllowOverlap

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
  , Raw.getBackgroundDrawList
  , Raw.getForegroundDrawList
  , Raw.imCol32
  , Raw.framerate
  , Raw.getTime
  , Raw.getFrameCount
  , calcTextSize

    -- * Types
  , module DearImGui.Enums
  , module DearImGui.Structs
  )
  where

-- base
import Control.Monad
  ( when )
import Data.Bool
import Data.Foldable
  ( foldl', for_, traverse_ )
import Foreign
import Foreign.C

-- dear-imgui
import DearImGui.Enums
import DearImGui.Internal.Text (Text)
import DearImGui.Structs
import qualified DearImGui.Internal.Text as Text
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DragDrop as Raw.DragDrop
import qualified DearImGui.Raw.Font as Raw.Font
import qualified DearImGui.Raw.ListClipper as Raw.ListClipper

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

-- | Get the compiled version string e.g. "1.80 WIP" (essentially the value for
-- @IMGUI_VERSION@ from the compiled version of @imgui.cpp@).
getVersion :: MonadIO m => m Text
getVersion = liftIO do
  Raw.getVersion >>= Text.peekCString

-- | Send text to logs.
logText :: MonadIO m => Text -> m ()
logText t = liftIO do
  Text.withCString t Raw.logText

-- | Push window to the stack and start appending to it.
--
-- Returns 'False' to indicate the window is collapsed or fully clipped, so you
-- may early out and omit submitting anything to the window. Always call a
-- matching 'end' for each 'begin' call, regardless of its return value!
--
-- Wraps @ImGui::Begin()@ with default options.
begin :: MonadIO m => Text -> m Bool
begin name = liftIO do
  Text.withCString name \namePtr ->
    Raw.begin namePtr Nothing Nothing

-- | Append items to a window.
--
-- Action will get 'False' if the window is collapsed or fully clipped.
--
-- You may append multiple times to the same window during the same frame
-- by calling 'withWindow' in multiple places.
withWindow :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withWindow name = bracket (begin name) (const Raw.end)

-- | Append items to a window unless it is collapsed or fully clipped.
--
-- You may append multiple times to the same window during the same frame
-- by calling 'withWindowOpen' in multiple places.
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
withCloseableWindow :: (HasSetter ref Bool, MonadUnliftIO m) => Text -> ref -> m () -> m ()
withCloseableWindow name ref action = bracket open close (`when` action)
  where
    open = liftIO do
      with 1 \boolPtr -> do
        Text.withCString name \namePtr -> do
          isVisible <- Raw.begin namePtr (Just boolPtr) Nothing
          isOpen <- peek boolPtr
          when (isOpen == 0) $ ref $=! False
          pure isVisible

    close = liftIO . const Raw.end

-- | Append items to a fullscreen window.
--
-- The action runs inside a window that is set to behave as a backdrop.
-- It has no typical window decorations, ignores events and does not jump to front.
--
-- You may append multiple times to it during the same frame
-- by calling 'withFullscreen' in multiple places.
withFullscreen :: MonadUnliftIO m => m () -> m ()
withFullscreen action = bracket open close (`when` action)
  where
    open = liftIO do
      Raw.setNextWindowFullscreen
      Text.withCString "FullScreen" \namePtr ->
        Raw.begin namePtr (Just nullPtr) (Just fullscreenFlags)

    close = liftIO . const Raw.end

fullscreenFlags :: ImGuiWindowFlags
fullscreenFlags = foldl' (.|.) zeroBits
  [ ImGuiWindowFlags_NoBackground
  , ImGuiWindowFlags_NoBringToFrontOnFocus
  , ImGuiWindowFlags_NoDecoration
  , ImGuiWindowFlags_NoFocusOnAppearing
  , ImGuiWindowFlags_NoMove
  , ImGuiWindowFlags_NoResize
  , ImGuiWindowFlags_NoSavedSettings
  , ImGuiWindowFlags_NoScrollbar
  , ImGuiWindowFlags_NoScrollWithMouse
  , ImGuiWindowFlags_NoTitleBar
  ]


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
beginChild :: MonadIO m => Text -> ImVec2 -> Bool -> ImGuiWindowFlags -> m Bool
beginChild name size border flags = liftIO do
  Text.withCString name \namePtr ->
    with size \sizePtr ->
      Raw.beginChild namePtr sizePtr (bool 0 1 border) flags

-- | Action wrapper for child windows.
--
-- Action will get 'False' if the child region is collapsed or fully clipped.
withChild :: MonadUnliftIO m => Text -> ImVec2 -> Bool -> ImGuiWindowFlags -> (Bool -> m a) -> m a
withChild name size border flags = bracket (beginChild name size border flags) (const Raw.endChild)

-- | Action-skipping wrapper for child windows.
--
-- Action will be skipped if the child region is collapsed or fully clipped.
withChildOpen :: MonadUnliftIO m => Text -> ImVec2 -> Bool -> ImGuiWindowFlags -> m () -> m ()
withChildOpen name size border flags action =
  withChild name size border flags (`when` action)

-- | Action wrapper to run in a context of another child window addressed by its name.
--
-- Action will get 'False' if the child region is collapsed or fully clipped.
withChildContext :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withChildContext name action =
  bracket
    (liftIO $ Text.withCString name Raw.beginChildContext)
    (const Raw.endChild)
    action


-- | Plain text.
text :: MonadIO m => Text -> m ()
text t = liftIO do
  Text.withCString t \textPtr ->
    Raw.textUnformatted textPtr Nothing

-- | Colored text.
textColored :: (HasGetter ref ImVec4, MonadIO m) => ref -> Text -> m ()
textColored ref t = liftIO do
  currentValue <- get ref
  with currentValue \refPtr ->
    Text.withCString t $ Raw.textColored refPtr

-- | Plain text in a "disabled" color according to current style.
textDisabled :: MonadIO m => Text -> m ()
textDisabled t = liftIO do
  Text.withCString t Raw.textDisabled

-- | Plain text with a word-wrap capability.
--
-- Note that this won't work on an auto-resizing window if there's no other widgets to extend the window width,
-- you may need to set a size using 'setNextWindowSize'.
textWrapped :: MonadIO m => Text -> m ()
textWrapped t = liftIO do
  Text.withCString t Raw.textWrapped

-- | Label+text combo aligned to other label+value widgets.
labelText :: MonadIO m => Text -> Text -> m ()
labelText label t = liftIO do
  Text.withCString label \labelPtr ->
    Text.withCString t \textPtr ->
      Raw.labelText labelPtr textPtr

-- | Text with a little bullet aligned to the typical tree node.
bulletText :: MonadIO m => Text -> m ()
bulletText t = liftIO do
  Text.withCString t Raw.bulletText

-- | Text with an horizontal line.
separatorText :: MonadIO m => Text -> m ()
separatorText t = liftIO do
  Text.withCString t Raw.separatorText

-- | Shortcut for a labelled Bool.
valueBool :: MonadIO m => Text -> Bool -> m ()
valueBool t b = liftIO do
  Text.withCString t \tPtr -> Raw.valueBool tPtr (bool 0 1 b)

-- | Shortcut for a labelled Int.
valueInt32 :: MonadIO m => Text -> Int32 -> m ()
valueInt32 t i = liftIO do
  Text.withCString t \tPtr -> Raw.valueInt tPtr (CInt i)

-- | Shortcut for a labelled Word.
valueWord32 :: MonadIO m => Text -> Word32 -> m ()
valueWord32 t w = liftIO do
  Text.withCString t \tPtr -> Raw.valueUInt tPtr (CUInt w)

-- | Shortcut for a labelled Float.
valueFloat :: MonadIO m => Text -> Float -> Text -> m ()
valueFloat t f format = liftIO do
  Text.withCString t \tPtr ->
    Text.withCString format \formatPtr ->
      Raw.valueFloat tPtr (CFloat f) formatPtr

-- | A button. Returns 'True' when clicked.
--
-- Wraps @ImGui::Button()@.
button :: MonadIO m => Text -> m Bool
button label = liftIO do
  Text.withCString label Raw.button


-- | Button with @FramePadding=(0,0)@ to easily embed within text.
--
-- Wraps @ImGui::SmallButton()@.
smallButton :: MonadIO m => Text -> m Bool
smallButton label = liftIO do
  Text.withCString label Raw.smallButton


-- | Flexible button behavior without the visuals.
--
-- Frequently useful to build custom behaviors using the public api
-- (along with IsItemActive, IsItemHovered, etc).
--
-- Wraps @ImGui::InvisibleButton()@.
invisibleButton :: MonadIO m => Text -> ImVec2 -> ImGuiButtonFlags -> m Bool
invisibleButton label size flags = liftIO do
  Text.withCString label \labelPtr ->
    with size \sizePtr ->
      Raw.invisibleButton labelPtr sizePtr flags


-- | Square button with an arrow shape.
--
-- Wraps @ImGui::ArrowButton()@.
arrowButton :: MonadIO m => Text -> ImGuiDir -> m Bool
arrowButton strId dir = liftIO do
  Text.withCString strId \strIdPtr ->
    Raw.arrowButton strIdPtr dir


-- | Wraps @ImGui::Checkbox()@.
checkbox :: (HasSetter ref Bool, HasGetter ref Bool, MonadIO m) => Text -> ref -> m Bool
checkbox label ref = liftIO do
  currentValue <- get ref
  with (bool 0 1 currentValue) \boolPtr -> do
    changed <- Text.withCString label \labelPtr ->
      Raw.checkbox labelPtr boolPtr

    when changed do
      newValue <- peek boolPtr
      ref $=! (newValue == 1)

    return changed


-- | Checkbox for a bit mask inside a signed value.
checkboxFlags :: (HasSetter ref Int32, HasGetter ref Int32, MonadIO m) => Text -> ref -> Int32 -> m Bool
checkboxFlags label ref flagsValue = liftIO do
  currentValue <- get ref
  Text.withCString label \labelPtr ->
    with (CInt currentValue) \flagsPtr -> do
      changed <- Raw.checkboxFlags labelPtr flagsPtr (CInt flagsValue)

      when changed do
        CInt newValue <- peek flagsPtr
        ref $=! newValue

      return changed


-- | Checkbox for a bit mask inside an unsigned value.
checkboxFlagsU :: (HasSetter ref Word32, HasGetter ref Word32, MonadIO m) => Text -> ref -> Word32 -> m Bool
checkboxFlagsU label ref flagsValue = liftIO do
  currentValue <- get ref
  Text.withCString label \labelPtr ->
    with (CUInt currentValue) \flagsPtr -> do
      changed <- Raw.checkboxFlagsU labelPtr flagsPtr (CUInt flagsValue)

      when changed do
        CUInt newValue <- peek flagsPtr
        ref $=! newValue

      return changed

radioButton :: MonadIO m => Text -> Bool -> m Bool
radioButton label b = liftIO do
  Text.withCString label \labelPtr ->
    Raw.radioButton labelPtr (bool 0 1 b)


radioButtonI :: (HasSetter ref Int32, HasGetter ref Int32, MonadIO m) => Text -> ref -> Int32 -> m Bool
radioButtonI label ref vButton = liftIO do
  currentValue <- get ref
  Text.withCString label \labelPtr ->
    with (CInt currentValue) \valuePtr -> do
      changed <- Raw.radioButtonI labelPtr valuePtr (CInt vButton)

      when changed do
        CInt newValue <- peek valuePtr
        ref $=! newValue

      return changed


progressBar :: MonadIO m => Float -> Maybe Text -> m ()
progressBar progress overlay = liftIO do
  Text.withCStringOrNull overlay \overlayPtr ->
    Raw.progressBar (CFloat progress) overlayPtr


-- | Begin creating a combo box with a given label and preview value.
--
-- Returns 'True' if the combo box is open. In this state, you should populate
-- the contents of the combo box - for example, by calling 'selectable'.
--
-- Only call 'endCombo' if 'beginCombo' returns 'True'!
--
-- Wraps @ImGui::BeginCombo()@.
beginCombo :: MonadIO m => Text -> Text -> m Bool
beginCombo label previewValue = liftIO $
  Text.withCString label        \labelPtr ->
  Text.withCString previewValue \previewValuePtr ->
  Raw.beginCombo labelPtr previewValuePtr

-- | Create a combo box with a given label and preview value.
--
-- Action will get 'True' if the combo box is open.
-- In this state, you should populate the contents of the combo box - for example, by calling 'selectable'.
withCombo :: MonadUnliftIO m => Text -> Text -> (Bool -> m a) -> m a
withCombo label previewValue =
  bracket (beginCombo label previewValue) (`when` Raw.endCombo)

-- | Create a combo box with a given label and preview value.
--
-- Action will be called if the combo box is open to populate the contents
-- of the combo box - for example, by calling 'selectable'.
withComboOpen :: MonadUnliftIO m => Text -> Text -> m () -> m ()
withComboOpen label previewValue action =
  withCombo label previewValue (`when` action)

-- | Wraps @ImGui::Combo()@.
combo :: (MonadIO m, HasGetter ref Int, HasSetter ref Int) => Text -> ref -> [Text] -> m Bool
combo label selectedIndex items = liftIO $ Managed.with m return
  where
    m = do
      i <- get selectedIndex

      cStrings <- traverse (\str -> Managed.managed (Text.withCString str)) items
      labelPtr <- Managed.managed $ Text.withCString label
      iPtr     <- Managed.managed $ with (fromIntegral i)

      liftIO $ withArrayLen cStrings \len itemsPtr -> do
        changed <- Raw.combo labelPtr iPtr itemsPtr (fromIntegral len)

        when changed do
          i' <- peek iPtr
          selectedIndex $=! fromIntegral i'

        return changed


-- | Wraps @ImGui::DragFloat()@
dragFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat desc ref speed minValue maxValue = liftIO do
  currentValue <- get ref
  with (realToFrac currentValue) \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.dragFloat descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      newValue <- peek floatPtr
      ref $=! realToFrac newValue

    return changed


-- | Wraps @ImGui::DragFloat2()@
dragFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat2 desc ref speed minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ realToFrac x, realToFrac y ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.dragFloat2 descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      [x', y'] <- peekArray 2 floatPtr
      ref $=! (realToFrac x', realToFrac y')

    return changed

-- | Wraps @ImGui::DragFloat3()@
dragFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat3 desc ref speed minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.dragFloat3 descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      [x', y', z'] <- peekArray 3 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z')

    return changed


-- | Wraps @ImGui::DragFloat4()@
dragFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => Text -> ref -> Float -> Float -> Float -> m Bool
dragFloat4 desc ref speed minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z, realToFrac u ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.dragFloat4 descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      [x', y', z', u'] <- peekArray 4 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z', realToFrac u')

    return changed

dragFloatRange2 :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> ref -> Float -> Float -> Float -> Text -> Text -> m Bool
dragFloatRange2 desc refMin refMax speed minValue maxValue minFmt maxFmt = liftIO do
  curMin <- get refMin
  curMax <- get refMax
  with (CFloat curMin) \minPtr ->
    with (CFloat curMax) \maxPtr -> do
      changed <-
        Text.withCString desc \descPtr ->
          Text.withCString minFmt \minFmtPtr ->
            Text.withCString maxFmt \maxFmtPtr ->
              Raw.dragFloatRange2
                descPtr
                minPtr maxPtr
                (CFloat speed) (CFloat minValue) (CFloat maxValue)
                minFmtPtr maxFmtPtr
                ImGuiSliderFlags_AlwaysClamp

      when changed do
        CFloat nextMin <- peek minPtr
        CFloat nextMax <- peek maxPtr
        refMin $=! nextMin
        refMax $=! nextMax

      return changed

-- | Wraps @ImGui::DragFloat()@
dragInt :: (MonadIO m, HasSetter ref Int, HasGetter ref Int) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt label ref speed minValue maxValue = liftIO do
  currentValue <- get ref
  with (fromIntegral currentValue) \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.dragInt
            labelPtr
            vPtr
            (CFloat speed)
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      newValue <- peek vPtr
      ref $=! fromIntegral newValue

    return changed

-- | Wraps @ImGui::DragInt2()@
dragInt2 :: (MonadIO m, HasSetter ref (Int, Int), HasGetter ref (Int, Int)) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt2 label ref speed minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ fromIntegral x, fromIntegral y ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.dragInt2
            labelPtr
            vPtr
            (CFloat speed)
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      [x', y'] <- peekArray 2 vPtr
      ref $=! (fromIntegral x', fromIntegral y')

    return changed

-- | Wraps @ImGui::DragInt3()@
dragInt3 :: (MonadIO m, HasSetter ref (Int, Int, Int), HasGetter ref (Int, Int, Int)) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt3 label ref speed minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.dragInt3
            labelPtr
            vPtr
            (CFloat speed)
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      [x', y', z'] <- peekArray 3 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z')

    return changed

-- | Wraps @ImGui::DragInt4()@
dragInt4 :: (MonadIO m, HasSetter ref (Int, Int, Int, Int), HasGetter ref (Int, Int, Int, Int)) => Text -> ref -> Float -> Int -> Int -> m Bool
dragInt4 label ref speed minValue maxValue = liftIO do
  (x, y, z, w) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z, fromIntegral w ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.dragInt4
            labelPtr
            vPtr
            (CFloat speed)
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      [x', y', z', w'] <- peekArray 3 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z', fromIntegral w')

    return changed

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
              Raw.dragIntRange2
                descPtr
                minPtr
                maxPtr
                (CFloat speed)
                (fromIntegral minValue)
                (fromIntegral maxValue)
                minFmtPtr maxFmtPtr
                ImGuiSliderFlags_AlwaysClamp

      when changed do
        nextMin <- peek minPtr
        nextMax <- peek maxPtr
        refMin $=! fromIntegral nextMin
        refMax $=! fromIntegral nextMax

      return changed

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
              Raw.dragScalar
                labelPtr
                dataType
                dataPtr
                (CFloat vSpeed)
                minPtr
                maxPtr
                formatPtr
                flags

        when changed do
          newValue <- peek dataPtr
          ref $=! newValue

        return changed

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
              Raw.dragScalarN
                labelPtr
                dataType
                dataPtr
                (fromIntegral components)
                (CFloat vSpeed)
                minPtr
                maxPtr
                formatPtr
                flags

        when changed do
          newValue <- peekArray components dataPtr
          ref $=! newValue

        return changed

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
              Raw.sliderScalar
                labelPtr
                dataType
                dataPtr
                minPtr
                maxPtr
                formatPtr
                flags

        when changed do
          newValue <- peek dataPtr
          ref $=! newValue

        return changed

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
              Raw.sliderScalarN
                labelPtr
                dataType
                dataPtr
                (fromIntegral components)
                minPtr
                maxPtr
                formatPtr
                flags

        when changed do
          newValue <- peekArray components dataPtr
          ref $=! newValue

        return changed

-- | Wraps @ImGui::SliderFloat()@
sliderFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> m Bool
sliderFloat desc ref minValue maxValue = liftIO do
  currentValue <- get ref
  with (realToFrac currentValue) \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.sliderFloat descPtr floatPtr (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      newValue <- peek floatPtr
      ref $=! realToFrac newValue

    return changed

-- | Wraps @ImGui::SliderFloat2()@
sliderFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => Text -> ref -> Float -> Float -> m Bool
sliderFloat2 desc ref minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ realToFrac x, realToFrac y ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.sliderFloat2 descPtr floatPtr (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      [x', y'] <- peekArray 2 floatPtr
      ref $=! (realToFrac x', realToFrac y')

    return changed

-- | Wraps @ImGui::SliderFloat3()@
sliderFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => Text -> ref -> Float -> Float -> m Bool
sliderFloat3 desc ref minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.sliderFloat3 descPtr floatPtr (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      [x', y', z'] <- peekArray 3 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z')

    return changed

-- | Wraps @ImGui::SliderFloat4()@
sliderFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => Text -> ref -> Float -> Float -> m Bool
sliderFloat4 desc ref minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z, realToFrac u ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.sliderFloat4 descPtr floatPtr (CFloat minValue) (CFloat maxValue) nullPtr

    when changed do
      [x', y', z', u'] <- peekArray 4 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z', realToFrac u')

    return changed

-- | Slider widget to select an angle in radians, while displaying degrees.
sliderAngle :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> m Bool
sliderAngle desc refRads minDegs maxDegs = liftIO do
  currentRads <- get refRads
  with (CFloat currentRads) \currentRadsPtr -> do
    changed <-
      Text.withCString desc \descPtr ->
        Text.withCString "%.0f deg" \formatPtr ->
          Raw.sliderAngle descPtr currentRadsPtr (CFloat minDegs) (CFloat maxDegs) formatPtr ImGuiSliderFlags_AlwaysClamp

    when changed do
      CFloat newRads <- peek currentRadsPtr
      refRads $=! newRads

    return changed

-- | Wraps @ImGui::SliderInt()@
sliderInt
  :: (MonadIO m, HasSetter ref Int, HasGetter ref Int)
  => Text -> ref -> Int -> Int -> m Bool
sliderInt label ref minValue maxValue = liftIO do
  currentValue <- get ref
  with (fromIntegral currentValue) \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.sliderInt
            labelPtr
            vPtr
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      newValue <- peek vPtr
      ref $=! fromIntegral newValue

    return changed

-- | Wraps @ImGui::SliderInt2()@
sliderInt2
  :: (MonadIO m, HasSetter ref (Int, Int), HasGetter ref (Int, Int))
  => Text -> ref -> Int -> Int -> m Bool
sliderInt2 label ref minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ fromIntegral x, fromIntegral y ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.sliderInt2
            labelPtr
            vPtr
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      [x', y'] <- peekArray 2 vPtr
      ref $=! (fromIntegral x', fromIntegral y')

    return changed

-- | Wraps @ImGui::SliderInt3()@
sliderInt3
  :: (MonadIO m, HasSetter ref (Int, Int, Int), HasGetter ref (Int, Int, Int))
  => Text -> ref -> Int -> Int -> m Bool
sliderInt3 label ref minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z ] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.sliderInt3
            labelPtr
            vPtr
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      [x', y', z'] <- peekArray 3 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z')

    return changed

-- | Wraps @ImGui::SliderInt4()@
sliderInt4
  :: (MonadIO m, HasSetter ref (Int, Int, Int, Int), HasGetter ref (Int, Int, Int, Int))
  => Text -> ref -> Int -> Int -> m Bool
sliderInt4 label ref minValue maxValue = liftIO do
  (x, y, z, w) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z, fromIntegral w] \vPtr -> do
    changed <-
      Text.withCString label \labelPtr ->
        Text.withCString "%d" \formatPtr ->
          Raw.sliderInt4
            labelPtr
            vPtr
            (fromIntegral minValue)
            (fromIntegral maxValue)
            formatPtr
            ImGuiSliderFlags_AlwaysClamp

    when changed do
      [x', y', z', w'] <- peekArray 4 vPtr
      ref $=! (fromIntegral x', fromIntegral y', fromIntegral z', fromIntegral w')

    return changed

vSliderFloat
  :: (HasSetter ref Float, HasGetter ref Float, MonadIO m)
  => Text -> ImVec2 -> ref -> Float -> Float -> m Bool
vSliderFloat label size ref minValue maxValue = liftIO do
  currentValue <- get ref

  with size \sizePtr ->
    with (CFloat currentValue) \dataPtr -> do
      changed <-
        Text.withCString label \labelPtr ->
          Text.withCString "%.3f" \formatPtr ->
            Raw.vSliderFloat
              labelPtr
              sizePtr
              dataPtr
              (CFloat minValue)
              (CFloat maxValue)
              formatPtr
              ImGuiSliderFlags_AlwaysClamp

      when changed do
        CFloat newValue <- peek dataPtr
        ref $=! newValue

      return changed

vSliderInt
  :: (HasSetter ref Int, HasGetter ref Int, MonadIO m)
  => Text -> ImVec2 -> ref -> Int -> Int -> m Bool
vSliderInt label size ref minValue maxValue = liftIO do
  currentValue <- get ref

  with size \sizePtr ->
    with (fromIntegral currentValue) \dataPtr -> do
      changed <-
        Text.withCString label \labelPtr ->
          Text.withCString "%d" \formatPtr ->
            Raw.vSliderInt
              labelPtr
              sizePtr
              dataPtr
              (fromIntegral minValue)
              (fromIntegral maxValue)
              formatPtr
              ImGuiSliderFlags_AlwaysClamp

      when changed do
        newValue <- peek dataPtr
        ref $=! fromIntegral newValue

      return changed

vSliderScalar
  :: (HasSetter ref a, HasGetter ref a, HasGetter range a, Storable a, MonadIO m)
  => Text -> ImVec2 -> ImGuiDataType -> ref -> range -> range -> Text -> ImGuiSliderFlags -> m Bool
vSliderScalar label size dataType ref refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with size \sizePtr ->
    with currentValue \dataPtr ->
      with minValue \minPtr ->
        with maxValue \maxPtr -> do
          changed <-
            Text.withCString label \labelPtr ->
              Text.withCString format \formatPtr ->
                Raw.vSliderScalar
                  labelPtr
                  sizePtr
                  dataType
                  dataPtr
                  minPtr
                  maxPtr
                  formatPtr
                  flags

          when changed do
            newValue <- peek dataPtr
            ref $=! newValue

          return changed


-- | Wraps @ImGui::InputText()@.
inputText :: (MonadIO m, HasSetter ref Text, HasGetter ref Text) => Text -> ref -> Int -> m Bool
inputText label ref bufSize =
  withInputString ref bufSize \bufPtrLen ->
      Text.withCString label \labelPtr ->
        Raw.inputText
          labelPtr
          bufPtrLen
          ImGuiInputTextFlags_None


-- | Wraps @ImGui::InputTextMultiline()@.
inputTextMultiline :: (MonadIO m, HasSetter ref Text, HasGetter ref Text) => Text -> ref -> Int -> ImVec2 -> m Bool
inputTextMultiline label ref bufSize size =
  withInputString ref bufSize \bufPtrLen ->
    Text.withCString label \labelPtr ->
      with size \sizePtr ->
        Raw.inputTextMultiline
          labelPtr
          bufPtrLen
          sizePtr
          ImGuiInputTextFlags_None


-- | Wraps @ImGui::InputTextWithHint()@.
inputTextWithHint :: (MonadIO m, HasSetter ref Text, HasGetter ref Text) => Text -> Text -> ref -> Int -> m Bool
inputTextWithHint label hint ref bufSize =
  withInputString ref bufSize \bufPtrLen ->
    Text.withCString label \labelPtr ->
      Text.withCString hint \hintPtr ->
        Raw.inputTextWithHint
          labelPtr
          hintPtr
          bufPtrLen
          ImGuiInputTextFlags_None


-- | Internal helper to prepare appropriately sized and encoded input buffer.
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

      changed <- action (bufPtr, bufSize)

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

inputFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => Text -> ref -> Float -> Float -> m Bool
inputFloat desc ref step stepFast = liftIO do
  currentValue <- get ref
  with (CFloat currentValue) \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputFloat descPtr floatPtr (CFloat step) (CFloat stepFast) nullPtr ImGuiInputTextFlags_None

    when changed do
      CFloat newValue <- peek floatPtr
      ref $=! newValue

    return changed

inputFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => Text -> ref -> m Bool
inputFloat2 desc ref = liftIO do
  (x, y) <- get ref
  withArray [ CFloat x, CFloat y ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputFloat2 descPtr floatPtr nullPtr ImGuiInputTextFlags_None

    when changed do
      [CFloat x', CFloat y'] <- peekArray 2 floatPtr
      ref $=! (x', y')

    return changed

inputFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => Text -> ref -> m Bool
inputFloat3 desc ref = liftIO do
  (x, y, z) <- get ref
  withArray [ CFloat x, CFloat y, CFloat z ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputFloat3 descPtr floatPtr nullPtr ImGuiInputTextFlags_None

    when changed do
      [CFloat x', CFloat y', CFloat z'] <- peekArray 3 floatPtr
      ref $=! (x', y', z')

    return changed

inputFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => Text -> ref -> m Bool
inputFloat4 desc ref = liftIO do
  (x, y, z, u) <- get ref
  withArray [ CFloat x, CFloat y, CFloat z, CFloat u ] \floatPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputFloat4 descPtr floatPtr nullPtr ImGuiInputTextFlags_None

    when changed do
      [CFloat x', CFloat y', CFloat z', CFloat u'] <- peekArray 4 floatPtr
      ref $=! (x', y', z', u')

    return changed

inputInt :: (MonadIO m, HasSetter ref Int32, HasGetter ref Int32) => Text -> ref -> Int32 -> Int32 -> m Bool
inputInt desc ref step stepFast = liftIO do
  currentValue <- get ref
  with (CInt currentValue) \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputInt descPtr intPtr (CInt step) (CInt stepFast) ImGuiInputTextFlags_None

    when changed do
      CInt newValue <- peek intPtr
      ref $=! newValue

    return changed

inputInt2 :: (MonadIO m, HasSetter ref (Int32, Int32), HasGetter ref (Int32, Int32)) => Text -> ref -> m Bool
inputInt2 desc ref = liftIO do
  (x, y) <- get ref
  withArray [ CInt x, CInt y ] \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputInt2 descPtr intPtr ImGuiInputTextFlags_None

    when changed do
      [CInt x', CInt y'] <- peekArray 2 intPtr
      ref $=! (x', y')

    return changed

inputInt3 :: (MonadIO m, HasSetter ref (Int32, Int32, Int32), HasGetter ref (Int32, Int32, Int32)) => Text -> ref -> m Bool
inputInt3 desc ref = liftIO do
  (x, y, z) <- get ref
  withArray [ CInt x, CInt y, CInt z ] \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputInt3 descPtr intPtr ImGuiInputTextFlags_None

    when changed do
      [CInt x', CInt y', CInt z'] <- peekArray 3 intPtr
      ref $=! (x', y', z')

    return changed

inputInt4 :: (MonadIO m, HasSetter ref (Int32, Int32, Int32, Int32), HasGetter ref (Int32, Int32, Int32, Int32)) => Text -> ref -> m Bool
inputInt4 desc ref = liftIO do
  (x, y, z, u) <- get ref
  withArray [ CInt x, CInt y, CInt z, CInt u ] \intPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.inputInt4 descPtr intPtr ImGuiInputTextFlags_None

    when changed do
      [CInt x', CInt y', CInt z', CInt u'] <- peekArray 4 intPtr
      ref $=! (x', y', z', u')

    return changed

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
              Raw.inputScalar
                labelPtr
                dataType
                dataPtr
                minPtr
                maxPtr
                formatPtr
                flags

        when changed do
          newValue <- peek dataPtr
          ref $=! newValue

        return changed

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
              Raw.inputScalarN
                labelPtr
                dataType
                dataPtr
                (fromIntegral components)
                minPtr
                maxPtr
                formatPtr
                flags

        when changed do
          newValue <- peekArray components dataPtr
          ref $=! newValue

        return changed

-- | Wraps @ImGui::ColorPicker3()@.
colorEdit3 :: (MonadIO m, HasSetter ref ImVec3, HasGetter ref ImVec3) => Text -> ref -> m Bool
colorEdit3 desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.colorEdit3 descPtr (castPtr refPtr) ImGuiColorEditFlags_None

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed


-- | Wraps @ImGui::ColorEdit3()@.
colorEdit4 :: (MonadIO m, HasSetter ref ImVec4, HasGetter ref ImVec4) => Text -> ref -> m Bool
colorEdit4 desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.colorEdit4 descPtr (castPtr refPtr) ImGuiColorEditFlags_None

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed


-- | Wraps @ImGui::ColorPicker3()@.
colorPicker3 :: (MonadIO m, HasSetter ref ImVec3, HasGetter ref ImVec3) => Text -> ref -> m Bool
colorPicker3 desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.colorPicker3 descPtr (castPtr refPtr) ImGuiColorEditFlags_None

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed

-- | Wraps @ImGui::ColorPicker3()@.
colorPicker4 :: (MonadIO m, HasSetter ref ImVec4, HasGetter ref ImVec4) => Text -> ref -> Maybe ImVec4 -> m Bool
colorPicker4 desc ref refColor = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      maybeWith with refColor \refColorPtr ->
        Raw.colorPicker4 descPtr (castPtr refPtr) ImGuiColorEditFlags_None (castPtr refColorPtr)

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed


-- | Display a color square/button, hover for details, return true when pressed.
--
-- Wraps @ImGui::ColorButton()@.
colorButton :: (MonadIO m, HasSetter ref ImVec4, HasGetter ref ImVec4) => Text -> ref -> m Bool
colorButton desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- Text.withCString desc \descPtr ->
      Raw.colorButton descPtr refPtr

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed

data TableOptions = TableOptions
  { tableFlags      :: ImGuiTableFlags
  , tableOuterSize  :: ImVec2
  , tableInnerWidth :: Float
  } deriving Show

defTableOptions :: TableOptions
defTableOptions = TableOptions
  { tableFlags      = ImGuiTableFlags_None
  , tableOuterSize  = ImVec2 0  0
  , tableInnerWidth = 0
  }
-- | Wraps @ImGui::BeginTable()@.
beginTable :: MonadIO m => TableOptions -> Text -> Int -> m Bool
beginTable TableOptions{..} label columns = liftIO do
  Text.withCString label \labelPtr ->
    with tableOuterSize \outerSizePtr ->
      Raw.beginTable labelPtr (fromIntegral columns) tableFlags outerSizePtr (CFloat tableInnerWidth)

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
withTable :: MonadUnliftIO m => TableOptions -> Text -> Int -> (Bool -> m a) -> m a
withTable options label columns =
  bracket (beginTable options label columns) (`when` Raw.endTable)

withTableOpen :: MonadUnliftIO m => TableOptions -> Text -> Int -> m () -> m ()
withTableOpen options label columns action =
  withTable options label columns (`when` action)

-- | Wraps @ImGui::TableNextRow()@ with 'defTableRowOptions'.
--   append into the first cell of a new row.
tableNextRow :: MonadIO m => m ()
tableNextRow = tableNextRowWith defTableRowOptions

data TableRowOptions = TableRowOptions
  { tableRowFlags     :: ImGuiTableRowFlags
  , tableRowMinHeight :: Float
  } deriving Show

defTableRowOptions :: TableRowOptions
defTableRowOptions = TableRowOptions
  { tableRowFlags     = ImGuiTableRowFlags_None
  , tableRowMinHeight = 0
  }

-- | Wraps @ImGui::TableNextRow()@ with explicit options.
tableNextRowWith :: MonadIO m => TableRowOptions -> m ()
tableNextRowWith TableRowOptions{..} = liftIO do
  Raw.tableNextRow tableRowFlags (CFloat tableRowMinHeight)

tableNextColumn :: MonadIO m => m () -> m ()
tableNextColumn action = Raw.tableNextColumn >>= (`when` action)

-- | Wraps @ImGui::TableSetColumnIndex()@.
--   append into the specified column. Return true when column is visible.
tableSetColumnIndex :: MonadIO m => Int -> m Bool
tableSetColumnIndex column = liftIO do
  Raw.tableSetColumnIndex (fromIntegral column)

data TableColumnOptions = TableColumnOptions
  { tableColumnFlags             :: ImGuiTableColumnFlags
  , tableColumnInitWidthOrWeight :: Float
  , tableColumnUserId            :: ImGuiID
  } deriving Show

defTableColumnOptions :: TableColumnOptions
defTableColumnOptions = TableColumnOptions
  { tableColumnFlags             = ImGuiTableColumnFlags_None
  , tableColumnInitWidthOrWeight = 0
  , tableColumnUserId            = 0
  }

-- | Wraps @ImGui::TableSetupColumn()@ using 'defTableColumnOptions'.
tableSetupColumn :: MonadIO m => Text -> m ()
tableSetupColumn = tableSetupColumnWith defTableColumnOptions

-- | Wraps @ImGui::TableSetupColumn() with explicit options@.
tableSetupColumnWith :: MonadIO m => TableColumnOptions -> Text -> m ()
tableSetupColumnWith TableColumnOptions{..} label = liftIO do
  Text.withCString label \labelPtr ->
    Raw.tableSetupColumn labelPtr tableColumnFlags (CFloat tableColumnInitWidthOrWeight) tableColumnUserId

-- | Wraps @ImGui::TableSetupScrollFreeze()@.
--   lock columns/rows so they stay visible when scrolled.
tableSetupScrollFreeze :: MonadIO m => Int -> Int -> m ()
tableSetupScrollFreeze cols rows = liftIO do
  Raw.tableSetupScrollFreeze (fromIntegral cols) (fromIntegral rows)

data TableSortingSpecs = TableSortingSpecs
  { tableSortingColumn  :: Int -- ^ Index of the column, starting at 0
  , tableSortingReverse :: Bool
  , tableSortingUserId  :: ImGuiID -- ^ User id of the column (if specified by a 'tableSetupColumn' call).
  } deriving (Eq, Ord, Show)

convertTableSortingSpecs :: ImGuiTableColumnSortSpecs -> TableSortingSpecs
convertTableSortingSpecs ImGuiTableColumnSortSpecs{..} =
  TableSortingSpecs
    { tableSortingColumn  = fromIntegral columnIndex
    , tableSortingReverse = sortDirection == ImGuiSortDirection_Descending
    , tableSortingUserId  = columnUserID
    }

-- | High-Level sorting. Returns of the underlying data should be sorted
--   and to what specification. Number of Specifications is mostly 0 or 1, but
--   can be more if 'ImGuiTableFlags_SortMulti' is enabled on the table.
--
--   The Bool only fires true for one frame on each sorting event and resets
--   automatically.
--
--   Must be called AFTER all columns are set up with 'tableSetupColumn'
--
--   Hint: Don't forget to set 'ImGuiTableFlags_Sortable' to enable sorting
--   on tables.
--
-- ==== __Example usage:__
--
-- > sortedData <- newIORef [("a","1"), ("b","2")]
-- >
-- > let sortable = defTableOptions { tableFlags = ImGuiTableFlags_Sortable }
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
withSortableTable :: MonadIO m => (Bool -> [TableSortingSpecs] -> m ()) -> m ()
withSortableTable action = do
  liftIO Raw.tableGetSortSpecs >>= \case
    Nothing ->
      -- XXX: The table is not sortable
      pure ()

    Just specsPtr -> do
      ImGuiTableSortSpecs{..} <- liftIO $ peek specsPtr
      let isDirty = 0 /= specsDirty
      columns <- liftIO $ peekArray (fromIntegral specsCount) specs

      action isDirty (map convertTableSortingSpecs columns)
      when isDirty $
        Raw.tableClearSortSpecsDirty specsPtr

-- | Wraps @ImGui::TableGetColumnCount()@.
--   return number of columns (value passed to BeginTable)
tableGetColumnCount :: MonadIO m => m Int
tableGetColumnCount =
  fromIntegral <$> Raw.tableGetColumnCount

-- | Wraps @ImGui::TableGetColumnIndex()@.
--   return current column index.
tableGetColumnIndex :: MonadIO m => m Int
tableGetColumnIndex =
  fromIntegral <$> Raw.tableGetColumnIndex

-- | Wraps @ImGui::TableGetRowIndex()@.
--   return current row index
tableGetRowIndex :: MonadIO m => m Int
tableGetRowIndex =
  fromIntegral <$> Raw.tableGetRowIndex

-- | Wraps @ImGui::TableGetColumnName
--   returns "" if column didn't have a name declared by TableSetupColumn
--   'Nothing' returns the current column name
tableGetColumnName :: MonadIO m => Maybe Int -> m Text
tableGetColumnName c = liftIO do
  Raw.tableGetColumnName (fromIntegral <$> c) >>= Text.peekCString

-- | Wraps @ImGui::TableGetRowIndex()@.
--    return column flags so you can query their Enabled/Visible/Sorted/Hovered
--    status flags.
--   'Nothing' returns the current column flags
tableGetColumnFlags :: MonadIO m => Maybe Int -> m ImGuiTableColumnFlags
tableGetColumnFlags =
  Raw.tableGetColumnFlags . fmap fromIntegral

-- | Wraps @ImGui::TableSetColumnEnabled()@.
--   change user accessible enabled/disabled state of a column. Set to false to
--   hide the column. User can use the context menu to change this themselves
--   (right-click in headers, or right-click in columns body with
--   'ImGuiTableFlags_ContextMenuInBody')
tableSetColumnEnabled :: MonadIO m => Int -> Bool -> m ()
tableSetColumnEnabled column_n v =
  Raw.tableSetColumnEnabled (fromIntegral column_n) (bool 0 1 v)

-- | Wraps @ImGui::TableSetBgColor()@.
--   change the color of a cell, row, or column.
--   See 'ImGuiTableBgTarget' flags for details.
--   'Nothing' sets the current row/column color
tableSetBgColor :: MonadIO m => ImGuiTableBgTarget -> ImU32 -> Maybe Int -> m ()
tableSetBgColor target color column_n =
 Raw.tableSetBgColor target color (fromIntegral <$> column_n)

-- | Wraps @ImGui::TreeNode()@.
treeNode :: MonadIO m => Text -> m Bool
treeNode label = liftIO do
  Text.withCString label Raw.treeNode

-- | Wraps @ImGui::TreeNodeEx()@.
treeNodeWith :: MonadIO m => Text -> ImGuiTreeNodeFlags -> m Bool
treeNodeWith label flags = liftIO do
  Text.withCString label (flip Raw.treeNodeEx flags)

-- | Wraps @ImGui::TreePush()@.
treePush :: MonadIO m => Text -> m ()
treePush label = liftIO do
  Text.withCString label Raw.treePush

getTreeNodeToLabelSpacing :: (MonadIO m) => m Float
getTreeNodeToLabelSpacing = liftIO do
  CFloat x <- Raw.getTreeNodeToLabelSpacing
  pure x

collapsingHeader :: (MonadIO m) => Text -> Maybe Bool -> m Bool
collapsingHeader label visible = liftIO do
  Text.withCString label \labelPtr ->
    maybeWith with (bool 0 1 <$> visible) \visiblePtr ->
      Raw.collapsingHeader labelPtr visiblePtr ImGuiTreeNodeFlags_None

-- | Wraps @ImGui::SetNextItemOpen()@.
setNextItemOpen :: MonadIO m => Bool -> m ()
setNextItemOpen is_open = Raw.setNextItemOpen (bool 0 1 is_open)

-- | Wraps @ImGui::Selectable()@ with default options.
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
  , flags    = ImGuiSelectableFlags_None
  , size     = ImVec2 0 0
  }

-- | Wraps @ImGui::Selectable()@ with explicit options.
selectableWith :: MonadIO m => SelectableOptions -> Text -> m Bool
selectableWith (SelectableOptions selected flags size) label = liftIO do
  with size \sizePtr ->
    Text.withCString label \labelPtr ->
      Raw.selectable labelPtr (bool 0 1 selected) flags sizePtr


listBox :: (MonadIO m, HasGetter ref Int, HasSetter ref Int) => Text -> ref -> [Text] -> m Bool
listBox label selectedIndex items = liftIO $ Managed.with m return
  where
    m = do
      i <- get selectedIndex

      cStrings <- traverse (\str -> Managed.managed (Text.withCString str)) items
      labelPtr <- Managed.managed $ Text.withCString label
      iPtr     <- Managed.managed $ with (fromIntegral i)

      liftIO $ withArrayLen cStrings \len itemsPtr -> do
        changed <- Raw.listBox labelPtr iPtr itemsPtr (fromIntegral len)

        when changed do
          i' <- peek iPtr
          selectedIndex $=! fromIntegral i'

        return changed

-- | Wraps @ImGui::PlotLines()@.
plotLines :: MonadIO m => Text -> [CFloat] -> m ()
plotLines label values = liftIO $
  withArrayLen values \len valuesPtr ->
    Text.withCString label \labelPtr ->
      Raw.plotLines labelPtr valuesPtr (fromIntegral len)

-- | Wraps @ImGui::PlotHistogram()@.
plotHistogram :: MonadIO m => Text -> [CFloat] -> m ()
plotHistogram label values = liftIO $
  withArrayLen values \len valuesPtr ->
    Text.withCString label \labelPtr ->
      Raw.plotHistogram labelPtr valuesPtr (fromIntegral len)

-- | Create a menu bar at the top of the screen and append to it.
--
-- The action will get 'False' if the menu is not visible.
withMainMenuBar :: MonadUnliftIO m => (Bool -> m a) -> m a
withMainMenuBar = bracket Raw.beginMainMenuBar (`when` Raw.endMainMenuBar)

-- | Create a menu bar at the top of the screen and append to it.
--
-- The action will be skipped if the menu is not visible.
withMainMenuBarOpen :: MonadUnliftIO m => m () -> m ()
withMainMenuBarOpen action =
  withMainMenuBar (`when` action)

-- | Append items to a window with MenuBar flag.
--
-- The action will get 'False' if the menu is not visible.
withMenuBar :: MonadUnliftIO m => (Bool -> m a) -> m a
withMenuBar = bracket Raw.beginMenuBar (`when` Raw.endMenuBar)

-- | Append items to a window with MenuBar flag.
--
-- The action will be skipped if the menu is not visible.
withMenuBarOpen :: MonadUnliftIO m => m () -> m ()
withMenuBarOpen action =
  withMenuBar (`when` action)

-- | Create a sub-menu entry.
--
-- Wraps @ImGui::BeginMenu()@.
beginMenu :: MonadIO m => Text -> m Bool
beginMenu label = liftIO do
  Text.withCString label Raw.beginMenu

-- | Create a sub-menu entry.
--
-- The action will get 'False' if the entry is not visible.
withMenu :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withMenu label = bracket (beginMenu label) (`when` Raw.endMenu)

-- | Create a sub-menu entry.
--
-- The action will be skipped if the entry is not visible.
withMenuOpen :: MonadUnliftIO m => Text -> m () -> m ()
withMenuOpen label action =
  withMenu label (`when` action)

-- | Return true when activated. Shortcuts are displayed for convenience but not
-- processed by ImGui at the moment
--
-- Wraps @ImGui::MenuItem()@
menuItem :: MonadIO m => Text -> m Bool
menuItem label = liftIO do
  Text.withCString label Raw.menuItem


-- | Create a @TabBar@ and start appending to it.
--
-- Wraps @ImGui::BeginTabBar@.
beginTabBar :: MonadIO m => Text -> ImGuiTabBarFlags -> m Bool
beginTabBar tabBarID flags = liftIO do
  Text.withCString tabBarID \ptr ->
    Raw.beginTabBar ptr flags

-- | Create a @TabBar@ and start appending to it.
--
-- The action will get 'False' if the Tab bar is not visible.
withTabBar :: MonadUnliftIO m => Text -> ImGuiTabBarFlags -> (Bool -> m a) -> m a
withTabBar tabBarID flags =
  bracket (beginTabBar tabBarID flags) (`when` Raw.endTabBar)

-- | Create a @TabBar@ and start appending to it.
--
-- The action will be skipped if the Tab bar is not visible.
withTabBarOpen :: MonadUnliftIO m => Text -> ImGuiTabBarFlags -> m () -> m ()
withTabBarOpen tabBarID flags action =
  withTabBar tabBarID flags (`when` action)

-- | Create a new tab. Returns @True@ if the tab is selected.
--
-- Wraps @ImGui::BeginTabItem@.
beginTabItem :: (MonadIO m, HasGetter ref Bool, HasSetter ref Bool) => Text -> ref -> ImGuiTabItemFlags -> m Bool
beginTabItem tabName ref flags = liftIO do
  currentValue <- get ref
  with (bool 0 1 currentValue) \refPtr -> do
    open <- Text.withCString tabName \ptrName ->
      Raw.beginTabItem ptrName refPtr flags

    newValue <- (0 /=) <$> peek refPtr
    when (newValue /= currentValue) do
      ref $=! newValue

    pure open

-- | Create a new tab.
--
-- The action will get 'True' if the tab is selected.
withTabItem :: (MonadUnliftIO m, HasGetter ref Bool, HasSetter ref Bool) => Text -> ref -> ImGuiTabItemFlags -> (Bool -> m a) -> m a
withTabItem tabName ref flags =
  bracket (beginTabItem tabName ref flags) (`when` Raw.endTabItem)

-- | Create a new tab.
--
-- The action will be skipped unless the tab is selected.
withTabItemOpen :: (MonadUnliftIO m, HasGetter ref Bool, HasSetter ref Bool) => Text -> ref -> ImGuiTabItemFlags -> m () -> m ()
withTabItemOpen tabName ref flags action =
  withTabItem tabName ref flags (`when` action)

-- | Create a tab that behaves like a button. Returns @True@ when clicked. Cannot be selected in the tab bar.
--
-- Wraps @ImGui.TabItemButton@.
tabItemButton :: MonadIO m => Text -> ImGuiTabItemFlags -> m Bool
tabItemButton tabName flags = liftIO do
  Text.withCString tabName \namePtr ->
    Raw.tabItemButton namePtr flags


-- | Notify the tab bar (or the docking system) that a tab/window is about to close.
-- Useful to reduce visual flicker on reorderable tab bars.
--
-- __For tab-bar__: call after 'beginTabBar' and before tab submission. Otherwise, call with a window name.
setTabItemClosed :: MonadIO m => Text -> m ()
setTabItemClosed tabName = liftIO do
  Text.withCString tabName Raw.setTabItemClosed

-- | Set a text-only tooltip if preceding item was hovered.
setItemTooltip :: MonadIO m => Text -> m ()
setItemTooltip tabName = liftIO do
  Text.withCString tabName Raw.setItemTooltip

-- | Create a tooltip if a previous item is hovered.
--
-- Those are windows that follow a mouse and don't take focus away.
-- Can contain any kind of items.
withItemTooltip ::  MonadUnliftIO m => m () -> m ()
withItemTooltip action = bracket Raw.beginItemTooltip (`when` Raw.endTooltip) (`when` action)

-- | Create a tooltip.
--
-- Those are windows that follow a mouse and don't take focus away.
-- Can contain any kind of items.
withTooltip ::  MonadUnliftIO m => m () -> m ()
withTooltip action = bracket Raw.beginItemTooltip (`when` Raw.endTooltip) (`when` action)


-- | Action wrapper for disabled blocks.
--
-- See 'Raw.beginDisabled' and 'Raw.endDisabled' for more info.
withDisabled :: (MonadUnliftIO m, HasGetter ref Bool) => ref -> m a -> m a
withDisabled disabledRef action = do
  disabled <- get disabledRef
  if disabled then bracket_ (Raw.beginDisabled 1) Raw.endDisabled action else action

-- | Begin a block that may be disabled. This disables all user interactions
-- and dims item visuals.
--
-- Always call a matching 'endDisabled' for each 'beginDisabled' call.
beginDisabled :: MonadIO m => Bool -> m ()
beginDisabled = Raw.beginDisabled . bool 0 1

-- | Returns 'True' if the popup is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopup()@
beginPopup :: MonadIO m => Text -> m Bool
beginPopup popupId = liftIO do
  Text.withCString popupId Raw.beginPopup

-- | Append items to a non-modal Popup.
--
-- Non-modal popups can be closed by clicking anywhere outside them,
-- or by pressing ESCAPE.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will get 'True' if the popup is open.
withPopup :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withPopup popupId = bracket (beginPopup popupId) (`when` Raw.endPopup)

-- | Append items to a non-modal Popup.
--
-- Non-modal popups can be closed by clicking anywhere outside them,
-- or by pressing ESCAPE.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will be called only if the popup is open.
withPopupOpen :: MonadUnliftIO m => Text -> m () -> m ()
withPopupOpen popupId action =
  withPopup popupId (`when` action)

-- | Returns 'True' if the modal is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopupModal()@
beginPopupModal :: MonadIO m => Text -> m Bool
beginPopupModal popupId = liftIO do
  Text.withCString popupId Raw.beginPopupModal

-- | Append items to a modal Popup.
--
-- Modal popups can be closed only with 'closeCurrentPopup'.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will get 'True' if the popup is open.
withPopupModal :: MonadUnliftIO m => Text -> (Bool -> m a) -> m a
withPopupModal popupId = bracket (beginPopupModal popupId) (`when` Raw.endPopup)

-- | Append intems to a modal Popup.
--
-- Modal popups can be closed only with 'closeCurrentPopup'.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will be called only if the popup is open.
withPopupModalOpen :: MonadUnliftIO m => Text -> m () -> m ()
withPopupModalOpen popupId action =
  withPopupModal popupId (`when` action)

beginPopupContextItem :: MonadIO m => Maybe Text -> ImGuiPopupFlags -> m Bool
beginPopupContextItem itemId flags = liftIO do
  Text.withCStringOrNull itemId \popupIdPtr ->
    Raw.beginPopupContextItem popupIdPtr flags

withPopupContextItem :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> (Bool -> m a) -> m a
withPopupContextItem popupId flags = bracket (beginPopupContextItem popupId flags) (`when` Raw.endPopup)

withPopupContextItemOpen :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> m () -> m ()
withPopupContextItemOpen popupId flags action = withPopupContextItem popupId flags (`when` action)

-- | Attach item context popup to right mouse button click on a last item.
itemContextPopup :: MonadUnliftIO m => m () -> m ()
itemContextPopup = withPopupContextItemOpen Nothing ImGuiPopupFlags_MouseButtonRight

beginPopupContextWindow :: MonadIO m => Maybe Text -> ImGuiPopupFlags -> m Bool
beginPopupContextWindow popupId flags = liftIO do
  Text.withCStringOrNull popupId \popupIdPtr ->
    Raw.beginPopupContextWindow popupIdPtr flags

withPopupContextWindow :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> (Bool -> m a) -> m a
withPopupContextWindow popupId flags = bracket (beginPopupContextWindow popupId flags) (`when` Raw.endPopup)

withPopupContextWindowOpen :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> m () -> m ()
withPopupContextWindowOpen popupId flags action = withPopupContextWindow popupId flags (`when` action)

-- | Attach item context popup to right mouse button click on a current window.
windowContextPopup :: MonadUnliftIO m => m () -> m ()
windowContextPopup = withPopupContextWindowOpen Nothing ImGuiPopupFlags_MouseButtonRight

beginPopupContextVoid :: MonadIO m => Maybe Text -> ImGuiPopupFlags -> m Bool
beginPopupContextVoid popupId flags = liftIO do
  Text.withCStringOrNull popupId \popupIdPtr ->
    Raw.beginPopupContextVoid popupIdPtr flags

withPopupContextVoid :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> (Bool -> m a) -> m a
withPopupContextVoid popupId flags = bracket (beginPopupContextVoid popupId flags) (`when` Raw.endPopup)

withPopupContextVoidOpen :: MonadUnliftIO m => Maybe Text -> ImGuiPopupFlags -> m () -> m ()
withPopupContextVoidOpen popupId flags action = withPopupContextVoid popupId flags (`when` action)

-- | Attach item context popup to right mouse button click outside of any windows.
voidContextPopup :: MonadUnliftIO m => m () -> m ()
voidContextPopup = withPopupContextWindowOpen Nothing ImGuiPopupFlags_MouseButtonRight


-- | Call to mark popup as open (don't call every frame!).
--
-- Wraps @ImGui::OpenPopup()@
openPopup :: MonadIO m => Text -> m ()
openPopup popupId = liftIO do
  Text.withCString popupId Raw.openPopup

-- | Opens a defined popup (i.e. defined with 'withPopup') on defined action.
--
-- Example:
--
-- > openPopupOnItemClick "myPopup" ImGuiPopupFlags_MouseButtonRight
--
-- Wraps @ImGui::OpenPopup()@
openPopupOnItemClick :: MonadIO m => Text -> ImGuiPopupFlags -> m ()
openPopupOnItemClick popupId flags = liftIO do
  Text.withCString popupId $ \idPtr ->
    Raw.openPopupOnItemClick idPtr flags

-- | Check if the popup is open at the current 'beginPopup' level of the popup stack.
isCurrentPopupOpen :: MonadIO m => Text -> m Bool
isCurrentPopupOpen popupId = liftIO do
  Text.withCString popupId $ \idPtr ->
    Raw.isPopupOpen idPtr ImGuiPopupFlags_None

-- | Check if *any* popup is open at the current 'beginPopup' level of the popup stack.
isAnyPopupOpen :: MonadIO m => Text -> m Bool
isAnyPopupOpen popupId = liftIO do
  Text.withCString popupId $ \idPtr ->
    Raw.isPopupOpen idPtr ImGuiPopupFlags_AnyPopupId

-- | Check if *any* popup is open at any level of the popup stack.
isAnyLevelPopupOpen :: MonadIO m => Text -> m Bool
isAnyLevelPopupOpen popupId = liftIO do
  Text.withCString popupId $ \idPtr ->
    Raw.isPopupOpen idPtr $
      ImGuiPopupFlags_AnyPopupId .|. ImGuiPopupFlags_AnyPopupLevel

getWindowWidth :: MonadIO m => m Float
getWindowWidth = liftIO do
  CFloat w <- Raw.getWindowWidth
  pure w

getWindowHeight :: MonadIO m => m Float
getWindowHeight = liftIO do
  CFloat w <- Raw.getWindowHeight
  pure w

-- | Set next window position. Call before `begin` Use pivot=(0.5,0.5) to center on given point, etc.
--
-- Wraps @ImGui::SetNextWindowPos()@
setNextWindowPos
  :: (MonadIO m, HasGetter ref ImVec2)
  => ref
  -> ImGuiCond
  -> Maybe ref -- XXX: the type should be distinct, but using `setNextWindowPos .. Nothing` is ambiguous resulting in bad UX.
  -> m ()
setNextWindowPos posRef cond pivotMaybe = liftIO do
  pos <- get posRef
  with pos $ \posPtr ->
    case pivotMaybe of
      Just pivotRef -> do
        pivot <- get pivotRef
        with pivot $ \pivotPtr ->
          Raw.setNextWindowPos posPtr cond (Just pivotPtr)
      Nothing ->
        Raw.setNextWindowPos posPtr cond Nothing

-- | Set next window size. Call before `begin`
--
-- Wraps @ImGui::SetNextWindowSize()@
setNextWindowSize :: (MonadIO m, HasGetter ref ImVec2) => ref -> ImGuiCond -> m ()
setNextWindowSize sizeRef cond = liftIO do
  size' <- get sizeRef
  with size' \sizePtr ->
    Raw.setNextWindowSize sizePtr cond

-- | Set next window content size (~ scrollable client area, which enforce the range of scrollbars). Not including window decorations (title bar, menu bar, etc.) nor WindowPadding. call before `begin`
--
-- Wraps @ImGui::SetNextWindowContentSize()@
setNextWindowContentSize :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
setNextWindowContentSize sizeRef = liftIO do
  size' <- get sizeRef
  with size' Raw.setNextWindowContentSize


-- | Set next window size limits. use -1,-1 on either X/Y axis to preserve the current size. Sizes will be rounded down.
--
-- Wraps @ImGui::SetNextWindowContentSize()@
setNextWindowSizeConstraints :: (MonadIO m, HasGetter ref ImVec2) => ref -> ref -> m ()
setNextWindowSizeConstraints sizeMinRef sizeMaxRef = liftIO do
  sizeMin <- get sizeMinRef
  sizeMax <- get sizeMaxRef
  with sizeMin \sizeMinPtr ->
    with sizeMax \sizeMaxPtr ->
      Raw.setNextWindowSizeConstraints sizeMinPtr sizeMaxPtr


-- | Set next window collapsed state. call before `begin`
--
-- Wraps @ImGui::SetNextWindowCollapsed()@
setNextWindowCollapsed :: (MonadIO m) => Bool -> ImGuiCond -> m ()
setNextWindowCollapsed b cond = liftIO do
  Raw.setNextWindowCollapsed (bool 0 1 b) cond

setNextWindowScroll :: (MonadIO m) => ImVec2 -> m ()
setNextWindowScroll scroll = liftIO do
  with scroll Raw.setNextWindowScroll

-- | Set next window background color alpha. helper to easily override the Alpha component of `ImGuiCol_WindowBg`, `ChildBg`, `PopupBg`. you may also use `ImGuiWindowFlags_NoBackground`.
--
-- Wraps @ImGui::SetNextWindowBgAlpha()@
setNextWindowBgAlpha :: (MonadIO m) => Float -> m ()
setNextWindowBgAlpha alpha = liftIO do
  Raw.setNextWindowBgAlpha (CFloat alpha)


-- | Add a dummy item of given size. unlike `invisibleButton`, `dummy` won't take the mouse click or be navigable into.
--
-- Wraps @ImGui::Dummy()@
dummy :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
dummy sizeRef = liftIO do
  size' <- get sizeRef
  with size' Raw.dummy

withIndent :: MonadUnliftIO m => Float -> m a -> m a
withIndent width =
  bracket_ (indent width) (unindent width)

-- | Move content position toward the right, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Indent()@
indent :: (MonadIO m) => Float -> m ()
indent indent_w = liftIO do
  Raw.indent (CFloat indent_w)


-- | Move content position back to the left, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Unindent()@
unindent :: (MonadIO m) => Float -> m ()
unindent f = liftIO do
  Raw.unindent (CFloat f)


-- | Affect large frame+labels widgets only.
--
-- Wraps @ImGui::SetNextItemWidth()@
setNextItemWidth :: (MonadIO m) => Float -> m ()
setNextItemWidth itemWidth = liftIO do
  Raw.setNextItemWidth (CFloat itemWidth)


withItemWidth :: MonadUnliftIO m => Float -> m a -> m a
withItemWidth width =
  bracket_ (pushItemWidth width) Raw.popItemWidth

-- Wraps @ImGui::PushItemWidth()@
pushItemWidth :: (MonadIO m) => Float -> m ()
pushItemWidth itemWidth = liftIO do
  Raw.pushItemWidth (CFloat itemWidth)

withTextWrapPos :: MonadUnliftIO m => Float -> m a -> m a
withTextWrapPos width =
  bracket_ (pushTextWrapPos width) Raw.popTextWrapPos

-- | Push word-wrapping position for Text commands.
--
-- Negative: no wrapping.
-- Zero: wrap to end of window (or column).
-- Positive: wrap at 'wrap_pos_x' position in window local space.
pushTextWrapPos :: (MonadIO m) => Float -> m ()
pushTextWrapPos wrapLocalPosX = liftIO do
  Raw.pushTextWrapPos (CFloat wrapLocalPosX)

-- | Lock horizontal starting position
--
-- Wraps @ImGui::BeginGroup()@ and @ImGui::EndGroup()@
withGroup :: MonadUnliftIO m => m a -> m a
withGroup = bracket_ Raw.beginGroup Raw.endGroup

-- | Set cursor position in window-local coordinates
--
-- Wraps @ImGui::SetCursorPos()@
setCursorPos :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
setCursorPos posRef = liftIO do
  pos <- get posRef
  with pos Raw.setCursorPos

setCursorPosX :: (MonadIO m) => Float -> m ()
setCursorPosX x = Raw.setCursorPosX (CFloat x)

setCursorPosY :: (MonadIO m) => Float -> m ()
setCursorPosY y = Raw.setCursorPosY (CFloat y)

setCursorScreenPos :: (MonadIO m) => ImVec2 -> m ()
setCursorScreenPos pos = liftIO do
  with pos Raw.setCursorScreenPos

getCursorPosX :: (MonadIO m) => m Float
getCursorPosX = liftIO do
  CFloat x <- Raw.getCursorPosX
  pure x

getCursorPosY :: (MonadIO m) => m Float
getCursorPosY = liftIO do
  CFloat y <- Raw.getCursorPosY
  pure y

getTextLineHeight :: (MonadIO m) => m Float
getTextLineHeight = liftIO do
  CFloat h <- Raw.getTextLineHeight
  pure h

getTextLineHeightWithSpacing :: (MonadIO m) => m Float
getTextLineHeightWithSpacing = liftIO do
  CFloat h <- Raw.getTextLineHeightWithSpacing
  pure h

getFrameHeight :: (MonadIO m) => m Float
getFrameHeight = liftIO do
  CFloat h <- Raw.getFrameHeight
  pure h

getFrameHeightWithSpacing :: (MonadIO m) => m Float
getFrameHeightWithSpacing = liftIO do
  CFloat h <- Raw.getFrameHeightWithSpacing
  pure h

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
withID :: (MonadUnliftIO m, ToID id) => id -> m a -> m a
withID i = bracket_ (liftIO $ pushID i) Raw.popID

-- | A supplementary class to match overloaded functions in C++ the library.
class ToID a where
  pushID :: MonadIO m => a -> m ()

instance ToID CInt where
  pushID = Raw.pushIDInt

instance ToID Int where
  pushID = Raw.pushIDInt . fromIntegral

instance ToID Integer where
  pushID = Raw.pushIDInt . fromInteger

instance {-# OVERLAPPABLE #-} ToID (Ptr a) where
  pushID = Raw.pushIDPtr

instance {-# OVERLAPPING #-} ToID (Ptr CChar) where
  pushID = Raw.pushIDStr

instance ToID (Ptr CChar, Int) where
  pushID = Raw.pushIDStrLen

instance ToID Text where
  pushID t = liftIO $ Text.withCStringLen t pushID

withStyleColor :: (MonadUnliftIO m, HasGetter ref ImVec4) => ImGuiCol -> ref -> m a -> m a
withStyleColor color ref =
  bracket_ (pushStyleColor color ref) (Raw.popStyleColor 1)

-- | Modify a style color by pushing to the shared stack.
--
-- Always use this if you modify the style after `newFrame`.
--
-- Wraps @ImGui::PushStyleColor()@
pushStyleColor :: (MonadIO m, HasGetter ref ImVec4) => ImGuiCol -> ref -> m ()
pushStyleColor col colorRef = liftIO do
  color <- get colorRef
  with color \colorPtr ->
    Raw.pushStyleColor col colorPtr

withStyleVar :: (MonadUnliftIO m, HasGetter ref ImVec2) => ImGuiStyleVar -> ref -> m a -> m a
withStyleVar style ref =
  bracket_ (pushStyleVar style ref) (Raw.popStyleVar 1)

-- | Allow/disable focusing using TAB/Shift-TAB, enabled by default but you can disable it for certain widgets.
withTabStop :: MonadUnliftIO m => Bool -> m a -> m a
withTabStop enabled =
  bracket_ (pushTabStop enabled) Raw.popTabStop

-- | Allow/disable focusing using TAB/Shift-TAB, enabled by default but you can disable it for certain widgets.
pushTabStop :: (MonadIO m) => Bool -> m ()
pushTabStop = Raw.pushTabStop . bool 0 1

-- | Modify a style variable by pushing to the shared stack.
--
-- Always use this if you modify the style after `newFrame`.
--
-- Wraps @ImGui::PushStyleVar()@
pushStyleVar :: (MonadIO m, HasGetter ref ImVec2) => ImGuiStyleVar -> ref -> m ()
pushStyleVar style valRef = liftIO do
  val <- get valRef
  with val \valPtr ->
    Raw.pushStyleVar style valPtr

-- | Remove style variable modifications from the shared stack
--
-- Wraps @ImGui::PopStyleVar()@
popStyleVar :: (MonadIO m) => Int -> m ()
popStyleVar n = liftIO do
  Raw.popStyleVar (fromIntegral n)

-- | Render widgets inside the block using provided font.
withFont :: MonadUnliftIO m => Raw.Font.Font -> m a -> m a
withFont font = bracket_ (Raw.Font.pushFont font) Raw.Font.popFont

-- | Attach drag-n-drop source with a payload to a preceding item.
--
-- A valid target should have a matching payload type.
--
-- Data is copied and retained by DearImGui.
-- Action is executed when the payload is accepted.
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
withDragDropTarget :: (MonadUnliftIO m, Storable a) => ImGuiDragDropFlags -> Text -> (a -> m ()) -> m ()
withDragDropTarget flags payloadType action =
  withRunInIO \run ->
    Raw.DragDrop.beginTarget >>= flip when do
      Text.withCString payloadType \typePtr -> do
        payload_ <- Raw.DragDrop.acceptPayload typePtr flags
        for_ payload_ \payload -> do
          dataPtr <- Raw.DragDrop.getData payload
          Foreign.maybePeek peek (castPtr dataPtr) >>= traverse_ (run . action)
      Raw.DragDrop.endTarget

-- | Like 'withDragDropSource', but only set payload type.
withDragDropSource_ :: (MonadUnliftIO m) => ImGuiDragDropFlags -> Text -> (Bool -> m ()) -> m ()
withDragDropSource_ flags payloadType action =
  withRunInIO \run ->
    Raw.DragDrop.beginSource flags >>= flip when do
      accepted <-
        Text.withCString payloadType \typePtr ->
          Raw.DragDrop.setPayload typePtr nullPtr 0 ImGuiCond_Once
      run $ action accepted
      Raw.DragDrop.endSource

-- | Like 'withDragDropTarget', but only set payload type.
--
-- Payload data is ignored.
withDragDropTarget_ :: (MonadUnliftIO m) => ImGuiDragDropFlags -> Text -> m () -> m ()
withDragDropTarget_ flags payloadType action =
  withRunInIO \run ->
    Raw.DragDrop.beginTarget >>= flip when do
      Text.withCString payloadType \typePtr -> do
        payload_ <- Raw.DragDrop.acceptPayload typePtr flags
        for_ payload_ (\_dataPtr -> run action)
      Raw.DragDrop.endTarget

-- | Like 'withDragDropSource', explicitly setting data ptr and size.
--
-- Suitable for data with dynamic lengths via @withCStringLen@-like functions.
withDragDropSourceData :: (MonadUnliftIO m, Integral len) => ImGuiDragDropFlags -> Text -> (Ptr a, len) -> (Bool -> m ()) -> m ()
withDragDropSourceData flags payloadType (dataPtr, dataSize) action =
  withRunInIO \run ->
    Raw.DragDrop.beginSource flags >>= flip when do
      accepted <-
        Text.withCString payloadType \typePtr ->
          Raw.DragDrop.setPayload typePtr dataPtr (fromIntegral dataSize) ImGuiCond_Once
      run $ action accepted
      Raw.DragDrop.endSource

-- | Like 'withDragDropTarget', getting raw data ptr and size.
--
-- Check the size, and pointer for NULLs etc.!
withDragDropTargetData :: (MonadUnliftIO m, Integral len) => ImGuiDragDropFlags -> Text -> ((Ptr a, len) -> m ()) -> m ()
withDragDropTargetData flags payloadType action =
  withRunInIO \run ->
    Raw.DragDrop.beginTarget >>= flip when do
      Text.withCString payloadType \typePtr -> do
        payload_ <- Raw.DragDrop.acceptPayload typePtr flags
        for_ payload_ \payload -> do
          dataPtr <- Raw.DragDrop.getData payload
          dataSize <- Raw.DragDrop.getDataSize payload
          run $ action (castPtr dataPtr, fromIntegral dataSize)
      Raw.DragDrop.endTarget

-- | Clips a large list of items
--
-- The requirements on @a@ are that they are all of the same height.
withListClipper :: (ClipItems t a, MonadUnliftIO m) => Maybe Float -> t a -> (a -> m ()) -> m ()
withListClipper itemHeight items action =
  bracket
    (liftIO $ throwIfNull "withListClipper: ListClipper allocation failed" Raw.ListClipper.new)
    Raw.ListClipper.delete
    step
  where
    itemHeight' = maybe (-1.0) CFloat itemHeight
    itemCount' = maybe maxBound fromIntegral (itemCount items)

    step clipper = do
      Raw.ListClipper.begin clipper itemCount' itemHeight'
      go clipper

    go clipper = do
      doStep <- Raw.ListClipper.step clipper
      when doStep do
        let
          startIndex = fromIntegral $ Raw.ListClipper.displayStart clipper
          endIndex   = fromIntegral $ Raw.ListClipper.displayEnd clipper
        stepItems action $
          clipItems startIndex endIndex items

        go clipper

calcTextSize :: MonadIO m => Text -> Bool -> Float -> m ImVec2
calcTextSize t hideAfterDoubleHash wrapWidth = liftIO do
  Text.withCStringLen t \(textPtr, textLen) ->
    Raw.calcTextSize textPtr (textPtr `plusPtr` textLen) (bool 0 1 hideAfterDoubleHash) (CFloat wrapWidth)

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
  itemCount (ClipRange _begin end) =
    Just $ fromEnum end

  clipItems clipBegin clipEnd (ClipRange oldBegin oldEnd) =
    ClipRange
      (toEnum $ max clipBegin $ fromEnum oldBegin)
      (toEnum $ min clipEnd $ fromEnum oldEnd)

  stepItems action (ClipRange start end) =
    mapM_ action [start .. end - 1]
