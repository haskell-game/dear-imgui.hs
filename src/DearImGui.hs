{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui

Main ImGui module, exporting the functions to create a GUI.
-}

module DearImGui
  ( -- * Context Creation and Access
    Raw.Context(..)
  , Raw.createContext
  , Raw.destroyContext

    -- * Main
  , Raw.newFrame
  , Raw.endFrame
  , Raw.render
  , Raw.DrawData(..)
  , Raw.getDrawData
  , Raw.checkVersion

    -- * Demo, Debug, Information
  , Raw.showDemoWindow
  , Raw.showMetricsWindow
  , Raw.showAboutWindow
  , Raw.showUserGuide
  , getVersion

    -- * Styles
  , Raw.styleColorsDark
  , Raw.styleColorsLight
  , Raw.styleColorsClassic

    -- * Windows
  , begin
  , Raw.end
  , setNextWindowPos
  , setNextWindowSize
  , setNextWindowContentSize
  , setNextWindowSizeConstraints
  , setNextWindowCollapsed
  , setNextWindowBgAlpha

    -- * Child Windows
  , beginChild
  , Raw.endChild

    -- * Parameter stacks
  , pushStyleColor
  , Raw.popStyleColor
  , pushStyleVar
  , popStyleVar

    -- * Cursor/Layout
  , Raw.separator
  , Raw.sameLine
  , Raw.newLine
  , Raw.spacing
  , dummy
  , indent
  , unindent
  , setNextItemWidth
  , pushItemWidth
  , Raw.popItemWidth
  , Raw.beginGroup
  , Raw.endGroup
  , setCursorPos
  , Raw.alignTextToFramePadding

    -- * Widgets
    -- ** Text
  , text

    -- ** Main
  , button
  , smallButton
  , arrowButton
  , checkbox
  , progressBar
  , Raw.bullet

    -- ** Combo Box
  , beginCombo
  , Raw.endCombo
  , combo

    -- ** Drag Sliders
  , dragFloat
  , dragFloat2
  , dragFloat3
  , dragFloat4

    -- ** Slider
  , sliderFloat
  , sliderFloat2
  , sliderFloat3
  , sliderFloat4

    -- ** Text Input
  , inputText

    -- * Color Editor/Picker
  , colorPicker3
  , colorButton

    -- * Trees
  , treeNode
  , treePush
  , Raw.treePop

    -- ** Selectables
  , selectable

    -- ** List Boxes
  , listBox

    -- * Data Plotting
  , plotHistogram

    -- ** Menus
  , Raw.beginMenuBar
  , Raw.endMenuBar
  , Raw.beginMainMenuBar
  , Raw.endMainMenuBar
  , beginMenu
  , Raw.endMenu
  , menuItem

    -- ** Tabs, tab bar
  , beginTabBar
  , Raw.endTabBar
  , beginTabItem
  , Raw.endTabItem
  , tabItemButton
  , setTabItemClosed

    -- * Tooltips
  , Raw.beginTooltip
  , Raw.endTooltip

    -- * Popups/Modals
  , beginPopup
  , beginPopupModal
  , Raw.endPopup
  , openPopup
  , Raw.closeCurrentPopup

    -- * Item/Widgets Utilities
  , Raw.isItemHovered

    -- * Types
  , module DearImGui.Enums
  , module DearImGui.Structs
  )
  where

-- base
import Control.Monad
  ( when )
import Data.Bool
import Foreign
import Foreign.C

-- dear-imgui
import DearImGui.Enums
import DearImGui.Structs

-- managed
import qualified Control.Monad.Managed as Managed

-- StateVar
import Data.StateVar
  ( HasGetter(get), HasSetter, ($=!) )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

import qualified DearImGui.Raw as Raw


-- | Get the compiled version string e.g. "1.80 WIP" (essentially the value for
-- @IMGUI_VERSION@ from the compiled version of @imgui.cpp@).
getVersion :: MonadIO m => m String
getVersion = liftIO do
  peekCString =<< Raw.getVersion


-- | Push window to the stack and start appending to it.
--
-- Returns 'False' to indicate the window is collapsed or fully clipped, so you
-- may early out and omit submitting anything to the window. Always call a
-- matching 'end' for each 'begin' call, regardless of its return value!
--
-- Wraps @ImGui::Begin()@.
begin :: MonadIO m => String -> m Bool
begin name = liftIO do
  withCString name Raw.begin


-- | Wraps @ImGui::BeginChild()@.
beginChild :: MonadIO m => String -> m Bool
beginChild name = liftIO do
  withCString name Raw.beginChild


-- | Formatted text.
--
-- Wraps @ImGui::Text()@.
text :: MonadIO m => String -> m ()
text t = liftIO do
  withCString t Raw.text


-- | A button. Returns 'True' when clicked.
--
-- Wraps @ImGui::Button()@.
button :: MonadIO m => String -> m Bool
button label = liftIO do
  withCString label Raw.button


-- | Button with @FramePadding=(0,0)@ to easily embed within text.
--
-- Wraps @ImGui::SmallButton()@.
smallButton :: MonadIO m => String -> m Bool
smallButton label = liftIO do
  withCString label Raw.smallButton


-- | Square button with an arrow shape.
--
-- Wraps @ImGui::ArrowButton()@.
arrowButton :: MonadIO m => String -> ImGuiDir -> m Bool
arrowButton strId dir = liftIO do
  withCString strId \strIdPtr ->
    Raw.arrowButton strIdPtr dir


-- | Wraps @ImGui::Checkbox()@.
checkbox :: (HasSetter ref Bool, HasGetter ref Bool, MonadIO m) => String -> ref -> m Bool
checkbox label ref = liftIO do
  currentValue <- get ref
  with (bool 0 1 currentValue) \boolPtr -> do
    changed <- withCString label \labelPtr ->
      Raw.checkbox labelPtr boolPtr

    when changed do
      newValue <- peek boolPtr
      ref $=! (newValue == 1)

    return changed


progressBar :: MonadIO m => Float -> Maybe String -> m ()
progressBar progress overlay = liftIO do
  withCStringOrNull overlay \overlayPtr ->
    Raw.progressBar (CFloat progress) overlayPtr


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
  Raw.beginCombo labelPtr previewValuePtr


-- | Wraps @ImGui::Combo()@.
combo :: (MonadIO m, HasGetter ref Int, HasSetter ref Int) => String -> ref -> [String] -> m Bool
combo label selectedIndex items = liftIO $ Managed.with m return
  where
    m = do
      i <- get selectedIndex

      cStrings <- traverse (\str -> Managed.managed (withCString str)) items
      labelPtr <- Managed.managed $ withCString label
      iPtr     <- Managed.managed $ with (fromIntegral i)

      liftIO $ withArrayLen cStrings \len itemsPtr -> do
        changed <- Raw.combo labelPtr iPtr itemsPtr (fromIntegral len)

        when changed do
          i' <- peek iPtr
          selectedIndex $=! fromIntegral i'

        return changed


-- | Wraps @ImGui::DragFloat()@
dragFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat desc ref speed minValue maxValue = liftIO do
  currentValue <- get ref
  with (realToFrac currentValue) \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.dragFloat descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue)

    when changed do
      newValue <- peek floatPtr
      ref $=! realToFrac newValue

    return changed


-- | Wraps @ImGui::DragFloat2()@
dragFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat2 desc ref speed minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ realToFrac x, realToFrac y ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.dragFloat2 descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue)

    when changed do
      [x', y'] <- peekArray 2 floatPtr
      ref $=! (realToFrac x', realToFrac y')

    return changed

-- | Wraps @ImGui::DragFloat3()@
dragFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat3 desc ref speed minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.dragFloat3 descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue)

    when changed do
      [x', y', z'] <- peekArray 3 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z')

    return changed


-- | Wraps @ImGui::DragFloat4()@
dragFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat4 desc ref speed minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z, realToFrac u ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.dragFloat4 descPtr floatPtr (CFloat speed) (CFloat minValue) (CFloat maxValue)

    when changed do
      [x', y', z', u'] <- peekArray 4 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z', realToFrac u')

    return changed


-- | Wraps @ImGui::SliderFloat()@
sliderFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => String -> ref -> Float -> Float -> m Bool
sliderFloat desc ref minValue maxValue = liftIO do
  currentValue <- get ref
  with (realToFrac currentValue) \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.sliderFloat descPtr floatPtr (CFloat minValue) (CFloat maxValue)

    when changed do
      newValue <- peek floatPtr
      ref $=! realToFrac newValue

    return changed


-- | Wraps @ImGui::SliderFloat2()@
sliderFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => String -> ref -> Float -> Float -> m Bool
sliderFloat2 desc ref minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ realToFrac x, realToFrac y ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.sliderFloat descPtr floatPtr (CFloat minValue) (CFloat maxValue)

    when changed do
      [x', y'] <- peekArray 2 floatPtr
      ref $=! (realToFrac x', realToFrac y')

    return changed


-- | Wraps @ImGui::SliderFloat3()@
sliderFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => String -> ref -> Float -> Float -> m Bool
sliderFloat3 desc ref minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.sliderFloat descPtr floatPtr (CFloat minValue) (CFloat maxValue)

    when changed do
      [x', y', z'] <- peekArray 3 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z')

    return changed


-- | Wraps @ImGui::SliderFloat4()@
sliderFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => String -> ref -> Float -> Float -> m Bool
sliderFloat4 desc ref minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z, realToFrac u ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.sliderFloat descPtr floatPtr (CFloat minValue) (CFloat maxValue)

    when changed do
      [x', y', z', u'] <- peekArray 4 floatPtr
      ref $=! (realToFrac x', realToFrac y', realToFrac z', realToFrac u')

    return changed


-- | Wraps @ImGui::InputText()@.
inputText :: (MonadIO m, HasSetter ref String, HasGetter ref String) => String -> ref -> Int -> m Bool
inputText desc ref refSize = liftIO do
  input <- get ref
  withCString input \ refPtr -> do
    withCString desc \ descPtr -> do
      let refSize' :: CInt
          refSize' = fromIntegral refSize
      changed <- Raw.inputText descPtr refPtr refSize'

      when changed do
        peekCString refPtr >>= ($=!) ref

      return changed


-- | Wraps @ImGui::ColorPicker3()@.
colorPicker3 :: (MonadIO m, HasSetter ref ImVec3, HasGetter ref ImVec3) => String -> ref -> m Bool
colorPicker3 desc ref = liftIO do
  ImVec3{x, y, z} <- get ref
  withArray (realToFrac <$> [x, y, z]) \refPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.colorPicker3 descPtr refPtr

    when changed do
      [x', y', z'] <- peekArray 3 refPtr
      ref $=! ImVec3 (realToFrac x') (realToFrac y') (realToFrac z')

    return changed


-- | Display a color square/button, hover for details, return true when pressed.
--
-- Wraps @ImGui::ColorButton()@.
colorButton :: (MonadIO m, HasSetter ref ImVec4, HasGetter ref ImVec4) => String -> ref -> m Bool
colorButton desc ref = liftIO do
  currentValue <- get ref
  with currentValue \refPtr -> do
    changed <- withCString desc \descPtr ->
      Raw.colorButton descPtr refPtr

    when changed do
      newValue <- peek refPtr
      ref $=! newValue

    return changed


-- | Wraps @ImGui::TreeNode()@.
treeNode :: MonadIO m => String -> m Bool
treeNode label = liftIO do
  withCString label Raw.treeNode


-- | Wraps @ImGui::TreePush()@.
treePush :: MonadIO m => String -> m ()
treePush label = liftIO do
  withCString label Raw.treePush


-- | Wraps @ImGui::Selectable()@.
selectable :: MonadIO m => String -> m Bool
selectable label = liftIO do
  withCString label Raw.selectable


listBox :: (MonadIO m, HasGetter ref Int, HasSetter ref Int) => String -> ref -> [String] -> m Bool
listBox label selectedIndex items = liftIO $ Managed.with m return
  where
    m = do
      i <- get selectedIndex

      cStrings <- traverse (\str -> Managed.managed (withCString str)) items
      labelPtr <- Managed.managed $ withCString label
      iPtr     <- Managed.managed $ with (fromIntegral i)

      liftIO $ withArrayLen cStrings \len itemsPtr -> do
        changed <- Raw.listBox labelPtr iPtr itemsPtr (fromIntegral len)

        when changed do
          i' <- peek iPtr
          selectedIndex $=! fromIntegral i'

        return changed


-- | Wraps @ImGui::PlotHistogram()@.
plotHistogram :: MonadIO m => String -> [CFloat] -> m ()
plotHistogram label values = liftIO $
  withArrayLen values \len valuesPtr ->
    withCString label \labelPtr ->
      Raw.plotHistogram labelPtr valuesPtr (fromIntegral len)


-- | Create a sub-menu entry.
--
-- Wraps @ImGui::BeginMenu()@.
beginMenu :: MonadIO m => String -> m Bool
beginMenu label = liftIO do
  withCString label Raw.beginMenu


-- | Return true when activated. Shortcuts are displayed for convenience but not
-- processed by ImGui at the moment
--
-- Wraps @ImGui::MenuItem()@
menuItem :: MonadIO m => String -> m Bool
menuItem label = liftIO do
  withCString label Raw.menuItem


-- | Create a @TabBar@ and start appending to it.
--
-- Wraps @ImGui::BeginTabBar@.
beginTabBar :: MonadIO m => String -> ImGuiTabBarFlags -> m Bool
beginTabBar tabBarID flags = liftIO do
  withCString tabBarID \ptr ->
    Raw.beginTabBar ptr flags


-- | Create a new tab. Returns @True@ if the tab is selected.
--
-- Wraps @ImGui::BeginTabItem@.
beginTabItem :: ( MonadIO m, HasGetter ref Bool, HasSetter ref Bool ) => String -> ref -> ImGuiTabBarFlags -> m Bool
beginTabItem tabName ref flags = liftIO do
  currentValue <- get ref
  with (bool 0 1 currentValue) \refPtr -> do
    open <- withCString tabName \ptrName ->
      Raw.beginTabItem ptrName refPtr flags

    newValue <- (0 /=) <$> peek refPtr
    when (newValue /= currentValue) do
      ref $=! newValue

    pure open


-- | Create a tab that behaves like a button. Returns @True@ when clicked. Cannot be selected in the tab bar.
--
-- Wraps @ImGui.TabItemButton@.
tabItemButton :: MonadIO m => String -> ImGuiTabItemFlags -> m Bool
tabItemButton tabName flags = liftIO do
  withCString tabName \namePtr ->
    Raw.tabItemButton namePtr flags


-- | Notify the tab bar (or the docking system) that a tab/window is about to close.
-- Useful to reduce visual flicker on reorderable tab bars.
--
-- __For tab-bar__: call after 'beginTabBar' and before tab submission. Otherwise, call with a window name.
setTabItemClosed :: MonadIO m => String -> m ()
setTabItemClosed tabName = liftIO do
  withCString tabName Raw.setTabItemClosed


-- | Returns 'True' if the popup is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopup()@
beginPopup :: MonadIO m => String -> m Bool
beginPopup popupId = liftIO do
  withCString popupId Raw.beginPopup


-- | Returns 'True' if the modal is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopupModal()@
beginPopupModal :: MonadIO m => String -> m Bool
beginPopupModal popupId = liftIO do
  withCString popupId Raw.beginPopupModal


-- | Call to mark popup as open (don't call every frame!).
--
-- Wraps @ImGui::OpenPopup()@
openPopup :: MonadIO m => String -> m ()
openPopup popupId = liftIO do
  withCString popupId Raw.openPopup


withCStringOrNull :: Maybe String -> (Ptr CChar -> IO a) -> IO a
withCStringOrNull Nothing k  = k nullPtr
withCStringOrNull (Just s) k = withCString s k


-- | Set next window position. Call before `begin` Use pivot=(0.5,0.5) to center on given point, etc.
--
-- Wraps @ImGui::SetNextWindowPos()@
setNextWindowPos :: (MonadIO m, HasGetter ref ImVec2) => ref -> ImGuiCond -> Maybe ref -> m ()
setNextWindowPos posRef cond pivotMaybe = liftIO do
  pos <- get posRef
  with pos $ \posPtr ->
    case pivotMaybe of
      Just pivotRef -> do
        pivot <- get pivotRef
        with pivot $ \pivotPtr ->
          Raw.setNextWindowPos posPtr cond pivotPtr
      Nothing ->
        Raw.setNextWindowPos posPtr cond nullPtr

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


-- Wraps @ImGui::PushItemWidth()@
pushItemWidth :: (MonadIO m) => Float -> m ()
pushItemWidth itemWidth = liftIO do
  Raw.pushItemWidth (CFloat itemWidth)


-- | Set cursor position in window-local coordinates
--
-- Wraps @ImGui::SetCursorPos()@
setCursorPos :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
setCursorPos posRef = liftIO do
  pos <- get posRef
  with pos Raw.setCursorPos


-- | Modify a style color by pushing to the shared stack. always use this if you modify the style after `newFrame`
--
-- Wraps @ImGui::PushStyleColor()@
pushStyleColor :: (MonadIO m, HasGetter ref ImVec4) => ImGuiCol -> ref -> m ()
pushStyleColor col colorRef = liftIO do
  color <- get colorRef
  with color \colorPtr ->
    Raw.pushStyleColor col colorPtr


-- | Modify a style variable by pushing to the shared stack. always use this if you modify the style after `newFrame`
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
