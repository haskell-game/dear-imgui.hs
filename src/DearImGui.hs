{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , withWindow
  , withWindowOpen
  , withFullscreen
  , fullscreenFlags

  , begin
  , Raw.end
  , setNextWindowPos
  , setNextWindowSize
  , Raw.setNextWindowFullscreen
  , setNextWindowContentSize
  , setNextWindowSizeConstraints
  , setNextWindowCollapsed
  , setNextWindowBgAlpha

    -- * Child Windows
  , withChild
  , withChildOpen
  , beginChild
  , Raw.endChild

    -- * Parameter stacks
  , withStyleColor
  , pushStyleColor
  , Raw.popStyleColor

  , withStyleVar
  , pushStyleVar
  , popStyleVar

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

  , withGroup
  , Raw.beginGroup
  , Raw.endGroup

  , setCursorPos
  , Raw.alignTextToFramePadding

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

    -- ** Main
  , button
  , smallButton
  , arrowButton
  , checkbox
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

    -- * Tooltips
  , withTooltip
  , Raw.beginTooltip
  , Raw.endTooltip

    -- * Popups/Modals
  , withPopup
  , withPopupOpen
  , beginPopup

  , withPopupModal
  , withPopupModalOpen
  , beginPopupModal

  , Raw.endPopup

  , openPopup
  , Raw.closeCurrentPopup

    -- * Item/Widgets Utilities
  , Raw.isItemHovered
  , Raw.wantCaptureMouse
  , Raw.wantCaptureKeyboard

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
  ( foldl' )
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

-- unliftio
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, bracket_)

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
-- Wraps @ImGui::Begin()@ with default options.
begin :: MonadIO m => String -> m Bool
begin name = liftIO do
  withCString name \namePtr ->
    Raw.begin namePtr nullPtr (ImGuiWindowFlags 0)

-- | Append items to a window.
--
-- Action will get 'False' if the window is collapsed or fully clipped.
--
-- You may append multiple times to the same window during the same frame
-- by calling 'withWindow' in multiple places.
withWindow :: MonadUnliftIO m => String -> (Bool -> m a) -> m a
withWindow name = bracket (begin name) (const Raw.end)

-- | Append items to a window unless it is collapsed or fully clipped.
--
-- You may append multiple times to the same window during the same frame
-- by calling 'withWindowOpen' in multiple places.
withWindowOpen :: MonadUnliftIO m => String -> m () -> m ()
withWindowOpen name action =
  withWindow name (`when` action)

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
      withCString "FullScreen" \namePtr ->
        Raw.begin namePtr nullPtr fullscreenFlags

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

-- | Wraps @ImGui::BeginChild()@.
beginChild :: MonadIO m => String -> m Bool
beginChild name = liftIO do
  withCString name Raw.beginChild

-- | Child windows used for self-contained independent scrolling/clipping regions
-- within a host window. Child windows can embed their own child.
--
-- Action will get 'False' if the child region is collapsed or fully clipped.
withChild :: MonadUnliftIO m => String -> (Bool -> m a) -> m a
withChild name = bracket (beginChild name) (const Raw.endChild)

-- | Child windows used for self-contained independent scrolling/clipping regions
-- within a host window. Child windows can embed their own child.
--
-- Action will be skipped if the child region is collapsed or fully clipped.
withChildOpen :: MonadUnliftIO m => String -> m () -> m ()
withChildOpen name action =
  withChild name (`when` action)

-- | Plain text.
text :: MonadIO m => String -> m ()
text t = liftIO do
  withCString t \textPtr ->
    Raw.textUnformatted textPtr nullPtr

-- | Colored text.
textColored :: (HasGetter ref ImVec4, MonadIO m) => ref -> String -> m ()
textColored ref t = liftIO do
  currentValue <- get ref
  with currentValue \refPtr ->
    withCString t $ Raw.textColored refPtr

-- | Plain text in a "disabled" color according to current style.
textDisabled :: MonadIO m => String -> m ()
textDisabled t = liftIO do
  withCString t Raw.textDisabled

-- | Plain text with a word-wrap capability.
--
-- Note that this won't work on an auto-resizing window if there's no other widgets to extend the window width,
-- you may need to set a size using 'setNextWindowSize'.
textWrapped :: MonadIO m => String -> m ()
textWrapped t = liftIO do
  withCString t Raw.textWrapped

-- | Label+text combo aligned to other label+value widgets.
labelText :: MonadIO m => String -> String -> m ()
labelText label t = liftIO do
  withCString label \labelPtr ->
    withCString t \textPtr ->
      Raw.labelText labelPtr textPtr

-- | Text with a little bullet aligned to the typical tree node.
bulletText :: MonadIO m => String -> m ()
bulletText t = liftIO do
  withCString t Raw.bulletText

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
-- Only call 'endCombo' if 'beginCombo' returns 'True'!
--
-- Wraps @ImGui::BeginCombo()@.
beginCombo :: MonadIO m => String -> String -> m Bool
beginCombo label previewValue = liftIO $
  withCString label        \labelPtr ->
  withCString previewValue \previewValuePtr ->
  Raw.beginCombo labelPtr previewValuePtr

-- | Create a combo box with a given label and preview value.
--
-- Action will get 'True' if the combo box is open.
-- In this state, you should populate the contents of the combo box - for example, by calling 'selectable'.
withCombo :: MonadUnliftIO m => String -> String -> (Bool -> m a) -> m a
withCombo label previewValue =
  bracket (beginCombo label previewValue) (`when` Raw.endCombo)

-- | Create a combo box with a given label and preview value.
--
-- Action will be called if the combo box is open to populate the contents
-- of the combo box - for example, by calling 'selectable'.
withComboOpen :: MonadUnliftIO m => String -> String -> m () -> m ()
withComboOpen label previewValue action =
  withCombo label previewValue (`when` action)

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

dragFloatRange2 :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => String -> ref -> ref -> Float -> Float -> Float -> String -> String -> m Bool
dragFloatRange2 desc refMin refMax speed minValue maxValue minFmt maxFmt = liftIO do
  curMin <- get refMin
  curMax <- get refMax
  with (CFloat curMin) \minPtr ->
    with (CFloat curMax) \maxPtr -> do
      changed <-
        withCString desc \descPtr ->
          withCString minFmt \minFmtPtr ->
            withCString maxFmt \maxFmtPtr ->
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
dragInt :: (MonadIO m, HasSetter ref Int, HasGetter ref Int) => String -> ref -> Float -> Int -> Int -> m Bool
dragInt label ref speed minValue maxValue = liftIO do
  currentValue <- get ref
  with (fromIntegral currentValue) \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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
dragInt2 :: (MonadIO m, HasSetter ref (Int, Int), HasGetter ref (Int, Int)) => String -> ref -> Float -> Int -> Int -> m Bool
dragInt2 label ref speed minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ fromIntegral x, fromIntegral y ] \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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
dragInt3 :: (MonadIO m, HasSetter ref (Int, Int, Int), HasGetter ref (Int, Int, Int)) => String -> ref -> Float -> Int -> Int -> m Bool
dragInt3 label ref speed minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z ] \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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
dragInt4 :: (MonadIO m, HasSetter ref (Int, Int, Int, Int), HasGetter ref (Int, Int, Int, Int)) => String -> ref -> Float -> Int -> Int -> m Bool
dragInt4 label ref speed minValue maxValue = liftIO do
  (x, y, z, w) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z, fromIntegral w ] \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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

dragIntRange2 :: (MonadIO m, HasSetter ref Int, HasGetter ref Int) => String -> ref -> ref -> Float -> Int -> Int -> String -> String -> m Bool
dragIntRange2 desc refMin refMax speed minValue maxValue minFmt maxFmt = liftIO do
  curMin <- get refMin
  curMax <- get refMax
  with (fromIntegral curMin) \minPtr ->
    with (fromIntegral curMax) \maxPtr -> do
      changed <-
        withCString desc \descPtr ->
          withCString minFmt \minFmtPtr ->
            withCString maxFmt \maxFmtPtr ->
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
  :: (HasSetter ref a, HasGetter ref a, Storable a, MonadIO m)
  => String -> ImGuiDataType -> ref -> Float -> ref -> ref -> String -> ImGuiSliderFlags -> m Bool
dragScalar label dataType ref vSpeed refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with currentValue \dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          withCString label \labelPtr ->
            withCString format \formatPtr ->
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
  :: (HasSetter valueRef [a], HasGetter valueRef [a], HasGetter rangeRef a, Storable a, MonadIO m)
  => String -> ImGuiDataType -> valueRef -> Float -> rangeRef -> rangeRef -> String -> ImGuiSliderFlags -> m Bool
dragScalarN label dataType ref vSpeed refMin refMax format flags = liftIO do
  currentValues <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  withArrayLen currentValues \components dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          withCString label \labelPtr ->
            withCString format \formatPtr ->
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
  :: (HasSetter ref a, HasGetter ref a, Storable a, MonadIO m)
  => String -> ImGuiDataType -> ref -> ref -> ref -> String -> ImGuiSliderFlags -> m Bool
sliderScalar label dataType ref refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with currentValue \dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          withCString label \labelPtr ->
            withCString format \formatPtr ->
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
  :: (HasSetter valueRef [a], HasGetter valueRef [a], HasGetter rangeRef a, Storable a, MonadIO m)
  => String -> ImGuiDataType -> valueRef -> rangeRef -> rangeRef -> String -> ImGuiSliderFlags -> m Bool
sliderScalarN label dataType ref refMin refMax format flags = liftIO do
  currentValues <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  withArrayLen currentValues \components dataPtr ->
    with minValue \minPtr ->
      with maxValue \maxPtr -> do
        changed <-
          withCString label \labelPtr ->
            withCString format \formatPtr ->
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

-- | Slider widget to select an angle in radians, while displaying degrees.
sliderAngle :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => String -> ref -> Float -> Float -> m Bool
sliderAngle desc refRads minDegs maxDegs = liftIO do
  currentRads <- get refRads
  with (CFloat currentRads) \currentRadsPtr -> do
    changed <-
      withCString desc \descPtr ->
        withCString "%.0f deg" \formatPtr ->
          Raw.sliderAngle descPtr currentRadsPtr (CFloat minDegs) (CFloat maxDegs) formatPtr ImGuiSliderFlags_AlwaysClamp

    when changed do
      CFloat newRads <- peek currentRadsPtr
      refRads $=! newRads

    return changed

-- | Wraps @ImGui::SliderInt()@
sliderInt
  :: (MonadIO m, HasSetter ref Int, HasGetter ref Int)
  => String -> ref -> Int -> Int -> m Bool
sliderInt label ref minValue maxValue = liftIO do
  currentValue <- get ref
  with (fromIntegral currentValue) \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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
  => String -> ref -> Int -> Int -> m Bool
sliderInt2 label ref minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ fromIntegral x, fromIntegral y ] \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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
  => String -> ref -> Int -> Int -> m Bool
sliderInt3 label ref minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z ] \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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
  => String -> ref -> Int -> Int -> m Bool
sliderInt4 label ref minValue maxValue = liftIO do
  (x, y, z, w) <- get ref
  withArray [ fromIntegral x, fromIntegral y, fromIntegral z, fromIntegral w] \vPtr -> do
    changed <-
      withCString label \labelPtr ->
        withCString "%d" \formatPtr ->
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
  => String -> ImVec2 -> ref -> Float -> Float -> m Bool
vSliderFloat label size ref minValue maxValue = liftIO do
  currentValue <- get ref

  with size \sizePtr ->
    with (CFloat currentValue) \dataPtr -> do
      changed <-
        withCString label \labelPtr ->
          withCString "%.3f" \formatPtr ->
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
  => String -> ImVec2 -> ref -> Int -> Int -> m Bool
vSliderInt label size ref minValue maxValue = liftIO do
  currentValue <- get ref

  with size \sizePtr ->
    with (fromIntegral currentValue) \dataPtr -> do
      changed <-
        withCString label \labelPtr ->
          withCString "%d" \formatPtr ->
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
  :: (HasSetter ref a, HasGetter ref a, Storable a, MonadIO m)
  => String -> ImVec2 -> ImGuiDataType -> ref -> ref -> ref -> String -> ImGuiSliderFlags -> m Bool
vSliderScalar label size dataType ref refMin refMax format flags = liftIO do
  currentValue <- get ref
  minValue <- get refMin
  maxValue <- get refMax

  with size \sizePtr ->
    with currentValue \dataPtr ->
      with minValue \minPtr ->
        with maxValue \maxPtr -> do
          changed <-
            withCString label \labelPtr ->
              withCString format \formatPtr ->
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
beginMenu :: MonadIO m => String -> m Bool
beginMenu label = liftIO do
  withCString label Raw.beginMenu

-- | Create a sub-menu entry.
--
-- The action will get 'False' if the entry is not visible.
withMenu :: MonadUnliftIO m => String -> (Bool -> m a) -> m a
withMenu label = bracket (beginMenu label) (`when` Raw.endMenu)

-- | Create a sub-menu entry.
--
-- The action will be skipped if the entry is not visible.
withMenuOpen :: MonadUnliftIO m => String -> m () -> m ()
withMenuOpen label action =
  withMenu label (`when` action)

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

-- | Create a @TabBar@ and start appending to it.
--
-- The action will get 'False' if the Tab bar is not visible.
withTabBar :: MonadUnliftIO m => String -> ImGuiTabBarFlags -> (Bool -> m a) -> m a
withTabBar tabBarID flags =
  bracket (beginTabBar tabBarID flags) (`when` Raw.endTabBar)

-- | Create a @TabBar@ and start appending to it.
--
-- The action will be skipped if the Tab bar is not visible.
withTabBarOpen :: MonadUnliftIO m => String -> ImGuiTabBarFlags -> m () -> m ()
withTabBarOpen tabBarID flags action =
  withTabBar tabBarID flags (`when` action)

-- | Create a new tab. Returns @True@ if the tab is selected.
--
-- Wraps @ImGui::BeginTabItem@.
beginTabItem :: (MonadIO m, HasGetter ref Bool, HasSetter ref Bool) => String -> ref -> ImGuiTabBarFlags -> m Bool
beginTabItem tabName ref flags = liftIO do
  currentValue <- get ref
  with (bool 0 1 currentValue) \refPtr -> do
    open <- withCString tabName \ptrName ->
      Raw.beginTabItem ptrName refPtr flags

    newValue <- (0 /=) <$> peek refPtr
    when (newValue /= currentValue) do
      ref $=! newValue

    pure open

-- | Create a new tab.
--
-- The action will get 'True' if the tab is selected.
withTabItem :: (MonadUnliftIO m, HasGetter ref Bool, HasSetter ref Bool) => String -> ref -> ImGuiTabBarFlags -> (Bool -> m a) -> m a
withTabItem tabName ref flags =
  bracket (beginTabItem tabName ref flags) (`when` Raw.endTabItem)

-- | Create a new tab.
--
-- The action will be skipped unless the tab is selected.
withTabItemOpen :: (MonadUnliftIO m, HasGetter ref Bool, HasSetter ref Bool) => String -> ref -> ImGuiTabBarFlags -> m () -> m ()
withTabItemOpen tabName ref flags action =
  withTabItem tabName ref flags (`when` action)

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

-- | Create a tooltip.
--
-- Those are windows that follow a mouse and don't take focus away.
-- Can contain any kind of items.
withTooltip ::  MonadUnliftIO m => m a -> m a
withTooltip = bracket_ Raw.beginTooltip Raw.endTooltip

-- | Returns 'True' if the popup is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopup()@
beginPopup :: MonadIO m => String -> m Bool
beginPopup popupId = liftIO do
  withCString popupId Raw.beginPopup

-- | Append intems to a non-modal Popup.
--
-- Non-modal popups can be closed by clicking anywhere outside them,
-- or by pressing ESCAPE.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will get 'True' if the popup is open.
withPopup :: MonadUnliftIO m => String -> (Bool -> m a) -> m a
withPopup popupId = bracket (beginPopup popupId) (`when` Raw.endPopup)

-- | Append intems to a non-modal Popup.
--
-- Non-modal popups can be closed by clicking anywhere outside them,
-- or by pressing ESCAPE.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will be called only if the popup is open.
withPopupOpen :: MonadUnliftIO m => String -> m () -> m ()
withPopupOpen popupId action =
  withPopup popupId (`when` action)

-- | Returns 'True' if the modal is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopupModal()@
beginPopupModal :: MonadIO m => String -> m Bool
beginPopupModal popupId = liftIO do
  withCString popupId Raw.beginPopupModal

-- | Append intems to a modal Popup.
--
-- Modal popups can be closed only with 'closeCurrentPopup'.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will get 'True' if the popup is open.
withPopupModal :: MonadUnliftIO m => String -> (Bool -> m a) -> m a
withPopupModal popupId = bracket (beginPopupModal popupId) (`when` Raw.endPopup)

-- | Append intems to a modal Popup.
--
-- Modal popups can be closed only with 'closeCurrentPopup'.
--
-- Visibility state is held internally instead of being held by the programmer.
--
-- The action will be called only if the popup is open.
withPopupModalOpen :: MonadUnliftIO m => String -> m () -> m ()
withPopupModalOpen popupId action =
  withPopupModal popupId (`when` action)

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

instance ToID String where
  pushID s = liftIO $ withCStringLen s pushID

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
