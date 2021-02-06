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
  , Begin(..)
  , end
  , setNextWindowPos
  , setNextWindowSize
  , setNextWindowContentSize
  , setNextWindowSizeConstraints
  , setNextWindowCollapsed
  , setNextWindowBgAlpha

    -- * Child Windows
  , beginChild
  , endChild

    -- * Parameter stacks
  , pushStyleColor
  , popStyleColor
  , pushStyleVar
  , popStyleVar

    -- * Cursor/Layout
  , separator
  , sameLine
  , newLine
  , spacing
  , dummy
  , indent
  , unindent
  , beginGroup
  , endGroup
  , setCursorPos
  , alignTextToFramePadding

    -- * Widgets
    -- ** Text
  , text

    -- ** Main
  , button
  , smallButton
  , arrowButton
  , checkbox
  , Checkbox(..)
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

    -- ** Slider
  , sliderFloat
  , sliderFloat2
  , sliderFloat3
  , sliderFloat4

    -- * Color Editor/Picker
  , colorPicker3
  , colorButton

    -- * Trees
  , treeNode
  , treePush
  , treePop

    -- ** Selectables
  , selectable

    -- ** List Boxes
  , listBox

    -- * Data Plotting
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
  , beginTooltip
  , endTooltip

    -- * Popups/Modals
  , beginPopup
  , beginPopupModal
  , endPopup
  , openPopup
  , closeCurrentPopup

    -- * Item/Widgets Utilities
  , isItemHovered

    -- * Types
  , module DearImGui.Enums
  , module DearImGui.Structs

    -- * TODO
  , toStateVar
  )
  where

-- base
import Data.Bool
import Data.Coerce 
  ( coerce )
import Data.Int 
  ( Int32 )
import Foreign
import Foreign.C

-- dear-imgui
import DearImGui.Context
  ( imguiContext )
import DearImGui.Enums
import DearImGui.Structs

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- managed
import qualified Control.Monad.Managed as Managed

-- StateVar
import Data.StateVar
  ( HasGetter(get), HasSetter, ($=!), mapStateVar, StateVar(..) )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )


C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
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
begin :: MonadIO m => Begin -> m Bool
begin Begin{ name, isOpen } = liftIO $
  withCString name \namePtr ->
  withMaybeStateVar isOpen \isOpenPtr -> do
    (0 /=) <$> [C.exp| bool { ImGui::Begin($(char* namePtr), $(bool* isOpenPtr)) } |]


data Begin = Begin
  { name :: String

  , isOpen :: Maybe (StateVar CBool)
    -- ^ When set shows a window-closing widget in the upper-right corner of the
    -- window, which clicking will set this 'Bool' reference to 'False' when
    -- clicked.
  }


-- | Pop window from the stack.
--
-- Wraps @ImGui::End()@.
end :: MonadIO m => m ()
end = liftIO do
  [C.exp| void { ImGui::End(); } |]


-- | Wraps @ImGui::BeginChild()@.
beginChild :: MonadIO m => String -> m Bool
beginChild name = liftIO do
  withCString name \namePtr ->
    (0 /=) <$> [C.exp| bool { ImGui::BeginChild($(char* namePtr)) } |]


-- | Wraps @ImGui::EndChild()@.
endChild :: MonadIO m => m ()
endChild = liftIO do
  [C.exp| void { ImGui::EndChild(); } |]


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
    [C.exp| void { Text("%s", $(char* textPtr)) } |]


-- | A button. Returns 'True' when clicked.
--
-- Wraps @ImGui::Button()@.
button :: MonadIO m => String -> m Bool
button label = liftIO do
  withCString label \labelPtr ->
    (0 /=) <$> [C.exp| bool { Button($(char* labelPtr)) } |]


-- | Button with @FramePadding=(0,0)@ to easily embed within text.
--
-- Wraps @ImGui::SmallButton()@.
smallButton :: MonadIO m => String -> m Bool
smallButton label = liftIO do
  withCString label \labelPtr ->
    (0 /=) <$> [C.exp| bool { SmallButton($(char* labelPtr)) } |]


-- | Square button with an arrow shape.
--
-- Wraps @ImGui::ArrowButton()@.
arrowButton :: MonadIO m => String -> ImGuiDir -> m Bool
arrowButton strId dir = liftIO do
  withCString strId \strIdPtr ->
    (0 /=) <$> [C.exp| bool { ArrowButton($(char* strIdPtr), $(ImGuiDir dir)) } |]


-- | Wraps @ImGui::Checkbox()@.
checkbox :: MonadIO m => Checkbox -> m Bool
checkbox Checkbox{ label, checked } = liftIO $
  withStateVar (mapStateVar cBoolToBool boolToCBool checked) \boolPtr ->
  withCString label \labelPtr ->
  cBoolToBool <$> [C.exp| bool { Checkbox($(char* labelPtr), $(bool* boolPtr)) } |]


data Checkbox = Checkbox
  { label   :: String
  , checked :: StateVar Bool
  }


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
  (0 /=) <$> [C.exp| bool { BeginCombo($(char* labelPtr), $(char* previewValuePtr)) } |]


-- | Only call 'endCombo' if 'beginCombo' returns 'True'!
--
-- Wraps @ImGui::EndCombo()@.
endCombo :: MonadIO m => m ()
endCombo = liftIO do
  [C.exp| void { EndCombo() } |]


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
        let len' = fromIntegral len
        [C.exp| bool { Combo($(char* labelPtr), $(int* iPtr), $(char** itemsPtr), $(int len')) }|] >>= \case
          0 -> return False
          _ -> do
            i' <- peek iPtr
            selectedIndex $=! fromIntegral i'
            return True


-- | Wraps @ImGui::DragFloat()@
dragFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat desc ref speed minValue maxValue = liftIO do
  currentValue <- get ref
  with (realToFrac currentValue) \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { DragFloat( $(char* descPtr), $(float *floatPtr), $(float speed'), $(float min'), $(float max')) } |]

    newValue <- peek floatPtr
    ref $=! realToFrac newValue

    return changed
  where
    min', max', speed' :: CFloat
    min'   = realToFrac minValue
    max'   = realToFrac maxValue
    speed' = realToFrac speed


-- | Wraps @ImGui::DragFloat2()@
dragFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat2 desc ref speed minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ realToFrac x, realToFrac y ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { DragFloat2( $(char* descPtr), $(float *floatPtr), $(float speed'), $(float min'), $(float max')) } |]

    [x', y'] <- peekArray 2 floatPtr
    ref $=! (realToFrac x', realToFrac y')

    return changed
  where
    min', max', speed' :: CFloat
    min'   = realToFrac minValue
    max'   = realToFrac maxValue
    speed' = realToFrac speed


-- | Wraps @ImGui::DragFloat3()@
dragFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat3 desc ref speed minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { DragFloat3( $(char* descPtr), $(float *floatPtr), $(float speed'), $(float min'), $(float max')) } |]

    [x', y', z'] <- peekArray 3 floatPtr
    ref $=! (realToFrac x', realToFrac y', realToFrac z')

    return changed
  where
    min', max', speed' :: CFloat
    min'   = realToFrac minValue
    max'   = realToFrac maxValue
    speed' = realToFrac speed


-- | Wraps @ImGui::DragFloat4()@
dragFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => String -> ref -> Float -> Float -> Float -> m Bool
dragFloat4 desc ref speed minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z, realToFrac u ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { DragFloat4( $(char* descPtr), $(float *floatPtr), $(float speed'), $(float min'), $(float max')) } |]

    [x', y', z', u'] <- peekArray 4 floatPtr
    ref $=! (realToFrac x', realToFrac y', realToFrac z', realToFrac u')

    return changed
  where
    min', max', speed' :: CFloat
    min'   = realToFrac minValue
    max'   = realToFrac maxValue
    speed' = realToFrac speed


-- | Wraps @ImGui::SliderFloat()@
sliderFloat :: (MonadIO m, HasSetter ref Float, HasGetter ref Float) => String -> ref -> Float -> Float -> m Bool
sliderFloat desc ref minValue maxValue = liftIO do
  currentValue <- get ref
  with (realToFrac currentValue) \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { SliderFloat( $(char* descPtr), $(float *floatPtr), $(float min'), $(float max')) } |]

    newValue <- peek floatPtr
    ref $=! realToFrac newValue

    return changed
  where
    min', max' :: CFloat
    min' = realToFrac minValue
    max' = realToFrac maxValue


-- | Wraps @ImGui::SliderFloat2()@
sliderFloat2 :: (MonadIO m, HasSetter ref (Float, Float), HasGetter ref (Float, Float)) => String -> ref -> Float -> Float -> m Bool
sliderFloat2 desc ref minValue maxValue = liftIO do
  (x, y) <- get ref
  withArray [ realToFrac x, realToFrac y ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { SliderFloat2( $(char* descPtr), $(float *floatPtr), $(float min'), $(float max')) } |]

    [x', y'] <- peekArray 2 floatPtr
    ref $=! (realToFrac x', realToFrac y')

    return changed
  where
    min', max' :: CFloat
    min' = realToFrac minValue
    max' = realToFrac maxValue


-- | Wraps @ImGui::SliderFloat3()@
sliderFloat3 :: (MonadIO m, HasSetter ref (Float, Float, Float), HasGetter ref (Float, Float, Float)) => String -> ref -> Float -> Float -> m Bool
sliderFloat3 desc ref minValue maxValue = liftIO do
  (x, y, z) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { SliderFloat3( $(char* descPtr), $(float *floatPtr), $(float min'), $(float max')) } |]

    [x', y', z'] <- peekArray 3 floatPtr
    ref $=! (realToFrac x', realToFrac y', realToFrac z')

    return changed
  where
    min', max' :: CFloat
    min' = realToFrac minValue
    max' = realToFrac maxValue


-- | Wraps @ImGui::SliderFloat4()@
sliderFloat4 :: (MonadIO m, HasSetter ref (Float, Float, Float, Float), HasGetter ref (Float, Float, Float, Float)) => String -> ref -> Float -> Float -> m Bool
sliderFloat4 desc ref minValue maxValue = liftIO do
  (x, y, z, u) <- get ref
  withArray [ realToFrac x, realToFrac y, realToFrac z, realToFrac u ] \floatPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /=) <$> [C.exp| bool { SliderFloat4( $(char* descPtr), $(float *floatPtr), $(float min'), $(float max')) } |]

    [x', y', z', u'] <- peekArray 4 floatPtr
    ref $=! (realToFrac x', realToFrac y', realToFrac z', realToFrac u')

    return changed
  where
    min', max' :: CFloat
    min' = realToFrac minValue
    max' = realToFrac maxValue


-- | Wraps @ImGui::ColorPicker3()@.
colorPicker3 :: (MonadIO m, HasSetter ref ImVec3, HasGetter ref ImVec3) => String -> ref -> m Bool
colorPicker3 desc ref = liftIO do
  ImVec3{x, y, z} <- get ref
  withArray (realToFrac <$> [x, y, z]) \refPtr -> do
    changed <- withCString desc \descPtr ->
      (0 /= ) <$> [C.exp| bool { ColorPicker3( $(char* descPtr), $(float *refPtr) ) } |]

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
      (0 /=) <$> [C.exp| bool { ColorButton( $(char* descPtr), *$(ImVec4 *refPtr) ) } |]

    newValue <- peek refPtr
    ref $=! newValue

    return changed

-- | Wraps @ImGui::TreeNode()@.
treeNode :: MonadIO m => String -> m Bool
treeNode label = liftIO do
  withCString label \labelPtr ->
    (0 /=) <$> [C.exp| bool { TreeNode($(char* labelPtr)) } |]


-- | Wraps @ImGui::TreePush()@.
treePush :: MonadIO m => String -> m ()
treePush label = liftIO do
  withCString label \labelPtr ->
    [C.exp| void { TreePush($(char* labelPtr)) } |]


-- | Wraps @ImGui::TreePop()@.
treePop :: MonadIO m => m ()
treePop = liftIO do
  [C.exp| void { TreePop() } |]


-- | Wraps @ImGui::Selectable()@.
selectable :: MonadIO m => String -> m Bool
selectable label = liftIO do
  withCString label \labelPtr ->
    (0 /=) <$> [C.exp| bool { Selectable($(char* labelPtr)) } |]

listBox :: (MonadIO m, HasGetter ref Int, HasSetter ref Int) => String -> ref -> [String] -> m Bool
listBox label selectedIndex items = liftIO $ Managed.with m return
  where
    m = do
      i <- get selectedIndex

      cStrings <- traverse (\str -> Managed.managed (withCString str)) items
      labelPtr <- Managed.managed $ withCString label
      iPtr     <- Managed.managed $ with (fromIntegral i)

      liftIO $ withArrayLen cStrings \len itemsPtr -> do
        let len' = fromIntegral len
        [C.exp| bool { ListBox($(char* labelPtr), $(int* iPtr), $(char** itemsPtr), $(int len')) }|] >>= \case
          0 -> return False
          _ -> do
            i' <- peek iPtr
            selectedIndex $=! fromIntegral i'
            return True


-- | Wraps @ImGui::PlotHistogram()@.
plotHistogram :: MonadIO m => String -> [CFloat] -> m ()
plotHistogram label values = liftIO $
  withArrayLen values \len valuesPtr ->
    withCString label \labelPtr -> do
      let c'len = fromIntegral len
      [C.exp| void { PlotHistogram($(char* labelPtr), $(float* valuesPtr), $(int c'len)) } |]


-- | Append to menu-bar of current window (requires 'ImGuiWindowFlagsMenuBar'
-- flag set on parent window).
--
-- Wraps @ImGui::BeginMenuBar()@.
beginMenuBar :: MonadIO m => m Bool
beginMenuBar = liftIO do
  (0 /=) <$> [C.exp| bool { BeginMenuBar() } |]


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
  (0 /=) <$> [C.exp| bool { BeginMainMenuBar() } |]


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
    (0 /=) <$> [C.exp| bool { BeginMenu($(char* labelPtr)) } |]


-- | Only call 'endMenu' if 'beginMenu' returns true!
--
-- Wraps @ImGui::EndMenu()@.
endMenu :: MonadIO m => m ()
endMenu = liftIO do
  [C.exp| void { EndMenu(); } |]


-- | Return true when activated. Shortcuts are displayed for convenience but not
-- processed by ImGui at the moment
--
-- Wraps @ImGui::MenuItem()@
menuItem :: MonadIO m => String -> m Bool
menuItem label = liftIO do
  withCString label \labelPtr ->
    (0 /=) <$> [C.exp| bool { MenuItem($(char* labelPtr)) } |]

-- | Create a @TabBar@ and start appending to it.
--
-- Wraps @ImGui::BeginTabBar@.
beginTabBar :: MonadIO m => String -> ImGuiTabBarFlags -> m Bool
beginTabBar tabBarID flags = liftIO do
  withCString tabBarID \ptr ->
    (0 /=) <$> [C.exp| bool { BeginTabBar($(char* ptr), $(ImGuiTabBarFlags flags) ) } |]

-- | Finish appending elements to a tab bar. Only call if 'beginTabBar' returns @True@.
--
-- Wraps @ImGui::EndTabBar@.
endTabBar :: MonadIO m => m ()
endTabBar = liftIO do
  [C.exp| void { EndTabBar(); } |]

-- | Create a new tab. Returns @True@ if the tab is selected.
--
-- Wraps @ImGui::BeginTabItem@.
beginTabItem :: ( MonadIO m, HasGetter ref Bool, HasSetter ref Bool ) => String -> ref -> ImGuiTabBarFlags -> m Bool
beginTabItem tabName ref flags = liftIO do
  currentValue <- get ref
  with ( bool 0 1 currentValue :: CBool ) \ refPtr -> do
    open <- withCString tabName \ ptrName ->
      (0 /=) <$> [C.exp| bool { BeginTabItem($(char* ptrName), $(bool* refPtr), $(ImGuiTabBarFlags flags) ) } |]
    newValue <- (0 /=) <$> peek refPtr
    ref $=! newValue
    pure open

-- | Finish appending elements to a tab. Only call if 'beginTabItem' returns @True@.
--
-- Wraps @ImGui::EndTabItem@.
endTabItem :: MonadIO m => m ()
endTabItem = liftIO do
  [C.exp| void { EndTabItem(); } |]

-- | Create a tab that behaves like a button. Returns @True@ when clicked. Cannot be selected in the tab bar.
--
-- Wraps @ImGui.TabItemButton@.
tabItemButton :: MonadIO m => String -> ImGuiTabItemFlags -> m Bool
tabItemButton tabName flags = liftIO do
  withCString tabName \ namePtr ->
    (0 /=) <$> [C.exp| bool { TabItemButton($(char* namePtr), $(ImGuiTabItemFlags flags) ) } |]

-- | Notify the tab bar (or the docking system) that a tab/window is about to close.
-- Useful to reduce visual flicker on reorderable tab bars.
--
-- __For tab-bar__: call after 'beginTabBar' and before tab submission. Otherwise, call with a window name.
setTabItemClosed :: MonadIO m => String -> m ()
setTabItemClosed tabName = liftIO do
  withCString tabName \ namePtr ->
    [C.exp| void { SetTabItemClosed($(char* namePtr)); } |]

-- | Begin/append a tooltip window to create full-featured tooltip (with any
-- kind of items).
--
-- Wraps @ImGui::BeginTooltip()@
beginTooltip :: MonadIO m => m ()
beginTooltip = liftIO do
  [C.exp| void { BeginTooltip() } |]


-- | Wraps @ImGui::EndTooltip()@
endTooltip :: MonadIO m => m ()
endTooltip = liftIO do
  [C.exp| void { EndTooltip() } |]


-- | Returns 'True' if the popup is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopup()@
beginPopup :: MonadIO m => String -> m Bool
beginPopup popupId = liftIO do
  withCString popupId \popupIdPtr ->
    (0 /=) <$> [C.exp| bool { BeginPopup($(char* popupIdPtr)) } |]


-- | Returns 'True' if the modal is open, and you can start outputting to it.
--
-- Wraps @ImGui::BeginPopupModal()@
beginPopupModal :: MonadIO m => String -> m Bool
beginPopupModal popupId = liftIO do
  withCString popupId \popupIdPtr ->
    (0 /=) <$> [C.exp| bool { BeginPopupModal($(char* popupIdPtr)) } |]


-- | Only call 'endPopup' if 'beginPopup' or 'beginPopupModal' returns 'True'!
--
-- Wraps @ImGui::BeginPopupModal()@
endPopup :: MonadIO m => m ()
endPopup = liftIO do
  [C.exp| void { EndPopup() } |]


-- | Call to mark popup as open (don't call every frame!).
--
-- Wraps @ImGui::OpenPopup()@
openPopup :: MonadIO m => String -> m ()
openPopup popupId = liftIO do
  withCString popupId \popupIdPtr ->
    [C.exp| void { OpenPopup($(char* popupIdPtr)) } |]


-- | Manually close the popup we have begin-ed into.
--
-- Wraps @ImGui::ClosePopup()@
closeCurrentPopup :: MonadIO m => m ()
closeCurrentPopup = liftIO do
  [C.exp| void { CloseCurrentPopup() } |]


-- | Is the last item hovered? (and usable, aka not blocked by a popup, etc.).
--
-- Wraps @ImGui::IsItemHovered()@
isItemHovered :: MonadIO m => m Bool
isItemHovered = liftIO do
  (0 /=) <$> [C.exp| bool { IsItemHovered() } |]


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
          [C.exp| void { SetNextWindowPos(*$(ImVec2 *posPtr), $(ImGuiCond cond), *$(ImVec2 *pivotPtr)) } |]
      Nothing ->
        [C.exp| void { SetNextWindowPos(*$(ImVec2 *posPtr), $(ImGuiCond cond)) } |]

-- | Set next window size. Call before `begin` 
--
-- Wraps @ImGui::SetNextWindowSize()@
setNextWindowSize :: (MonadIO m, HasGetter ref ImVec2) => ref -> ImGuiCond -> m ()
setNextWindowSize sizeRef cond = liftIO do
  size' <- get sizeRef
  with size' $ 
    \sizePtr ->[C.exp| void { SetNextWindowSize(*$(ImVec2 *sizePtr), $(ImGuiCond cond)) } |]

-- | Set next window content size (~ scrollable client area, which enforce the range of scrollbars). Not including window decorations (title bar, menu bar, etc.) nor WindowPadding. call before `begin`
--
-- Wraps @ImGui::SetNextWindowContentSize()@
setNextWindowContentSize :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
setNextWindowContentSize sizeRef = liftIO do
  size' <- get sizeRef
  with size' $ 
    \sizePtr ->[C.exp| void { SetNextWindowContentSize(*$(ImVec2 *sizePtr)) } |]

-- | Set next window size limits. use -1,-1 on either X/Y axis to preserve the current size. Sizes will be rounded down.
--
-- Wraps @ImGui::SetNextWindowContentSize()@
setNextWindowSizeConstraints :: (MonadIO m, HasGetter ref ImVec2) => ref -> ref -> m ()
setNextWindowSizeConstraints sizeMinRef sizeMaxRef = liftIO do
  sizeMin <- get sizeMinRef
  sizeMax <- get sizeMaxRef
  with sizeMin $ 
    \sizeMinPtr -> 
      with sizeMax $ \sizeMaxPtr -> 
        [C.exp| void { SetNextWindowSizeConstraints(*$(ImVec2 *sizeMinPtr), *$(ImVec2 *sizeMaxPtr)) } |]

-- | Set next window collapsed state. call before `begin`
--
-- Wraps @ImGui::SetNextWindowCollapsed()@
setNextWindowCollapsed :: (MonadIO m) => Bool -> ImGuiCond -> m ()
setNextWindowCollapsed b cond = liftIO do
  let b' = bool 0 1 b
  [C.exp| void { SetNextWindowCollapsed($(bool b'), $(ImGuiCond cond)) } |]

-- | Set next window background color alpha. helper to easily override the Alpha component of `ImGuiCol_WindowBg`, `ChildBg`, `PopupBg`. you may also use `ImGuiWindowFlags_NoBackground`.
--
-- Wraps @ImGui::SetNextWindowBgAlpha()@
setNextWindowBgAlpha :: (MonadIO m) => Float -> m ()
setNextWindowBgAlpha f = liftIO do
  let f' = coerce f
  [C.exp| void { SetNextWindowBgAlpha($(float f')) } |]

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
dummy :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
dummy sizeRef = liftIO do 
  size' <- get sizeRef
  with size' $ \ sizePtr -> [C.exp| void { Dummy(*$(ImVec2 *sizePtr)) } |]

-- | Move content position toward the right, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Indent()@
indent :: (MonadIO m) => Float -> m ()
indent indent_w = liftIO do
  let indent_w' = coerce indent_w
  [C.exp| void { Indent($(float indent_w')) } |]

-- | Move content position back to the left, by indent_w, or style.IndentSpacing if indent_w <= 0
--
-- Wraps @ImGui::Unindent()@
unindent :: (MonadIO m) => Float -> m ()
unindent f = liftIO do
  let f' = coerce f
  [C.exp| void { Unindent($(float f')) } |]

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

-- | Set cursor position in window-local coordinates
-- 
-- Wraps @ImGui::SetCursorPos()@
setCursorPos :: (MonadIO m, HasGetter ref ImVec2) => ref -> m ()
setCursorPos posRef = liftIO do 
  pos <- get posRef
  with pos $ \ posPtr -> [C.exp| void { SetCursorPos(*$(ImVec2 *posPtr)) } |]

-- | Modify a style color by pushing to the shared stack. always use this if you modify the style after `newFrame`
-- 
-- Wraps @ImGui::PushStyleColor()@
pushStyleColor :: (MonadIO m, HasGetter ref ImVec4) => ImGuiCol -> ref -> m ()
pushStyleColor col colorRef = liftIO do 
  color <- get colorRef
  with color $ \ colorPtr -> [C.exp| void { PushStyleColor($(ImGuiCol col), *$(ImVec4 *colorPtr)) } |]

-- | Remove style color modifications from the shared stack
-- 
-- Wraps @ImGui::PopStyleColor()@
popStyleColor :: (MonadIO m) => Int32 -> m ()
popStyleColor n = liftIO do
  let
    m :: CInt
    m = coerce n
  [C.exp| void { PopStyleColor($(int m)) } |]

-- | Modify a style variable by pushing to the shared stack. always use this if you modify the style after `newFrame`
-- 
-- Wraps @ImGui::PushStyleVar()@
pushStyleVar :: (MonadIO m, HasGetter ref ImVec2) => ImGuiStyleVar -> ref -> m ()
pushStyleVar style valRef = liftIO do 
  val <- get valRef
  with val $ \ valPtr -> [C.exp| void { PushStyleVar($(ImGuiStyleVar style), *$(ImVec2 *valPtr)) } |]

-- | Remove style variable modifications from the shared stack
-- 
-- Wraps @ImGui::PopStyleVar()@
popStyleVar :: (MonadIO m) => Int32 -> m ()
popStyleVar n = liftIO do
  let
    m :: CInt
    m = coerce n
  [C.exp| void { PopStyleVar($(int m)) } |]


withMaybeStateVar :: Storable x => Maybe (StateVar x) -> (Ptr x -> IO r) -> IO r
withMaybeStateVar Nothing k  = k nullPtr
withMaybeStateVar (Just r) k = withStateVar r k


withStateVar :: Storable x => StateVar x -> (Ptr x -> IO r) -> IO r
withStateVar r k = do
  x <- get r
  with x \xPtr -> do
    y  <- k xPtr

    x' <- peek xPtr
    r $=! x'

    return y


boolToCBool :: Bool -> CBool
boolToCBool = bool 0 1


cBoolToBool :: CBool -> Bool
cBoolToBool = (/= 0)


toStateVar :: (HasGetter r a, HasSetter r a) => r -> StateVar a
toStateVar r = StateVar (get r) (r $=!)
