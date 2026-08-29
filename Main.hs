{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.IORef
import qualified Data.Vector as Vector
import DearImGui
import qualified DearImGui.Raw.Enums.ImGuiCond as ImGuiCond
import qualified DearImGui.Raw.Enums.ImGuiDir as ImGuiDir
import qualified DearImGui.Raw.Enums.ImGuiTabBarFlags as ImGuiTabBarFlags
import qualified DearImGui.Raw.Enums.ImGuiTabItemFlags as ImGuiTabItemFlags
import qualified DearImGui.Impl.OpenGL3 as ImplGL3
import qualified DearImGui.Impl.SDL2 as ImplSDL2
import DearImGui.Internal.Text (pack)
import Control.Exception
import Graphics.GL
import SDL

main :: IO ()
main = do
  initializeAll

  bracket (createWindow "Hello, Dear ImGui!" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }) destroyWindow \w ->
    bracket (glCreateContext w) glDeleteContext \glContext ->
    bracket createContext destroyContext \_imguiContext ->
    ImplSDL2.withInitForOpenGL w glContext $
    ImplGL3.withInit Nothing do
      checkVersion
      styleColorsLight

      checked <- newIORef False
      color <- newIORef $ ImVec3 1 0 0
      slider <- newIORef (0.42, 0, 0.314)
      r <- newIORef 4
      pos <- newIORef $ ImVec2 64 64
      size' <- newIORef $ ImVec2 512 512
      selected <- newIORef 4
      tab1 <- newIORef True
      tab2 <- newIORef True
      loop w checked color slider r pos size' selected tab1 tab2


loop
  :: Window
  -> IORef Bool
  -> IORef ImVec3
  -> IORef (Float, Float, Float)
  -> IORef Int
  -> IORef ImVec2
  -> IORef ImVec2
  -> IORef Int
  -> IORef Bool
  -> IORef Bool
  -> IO ()
loop window checked color slider r pos size' selected tab1Ref tab2Ref = do
  shouldQuit <- checkEvents

  ImplGL3.newFrame
  ImplSDL2.newFrame
  newFrame

  -- showDemoWindow
  -- showMetricsWindow
  -- showAboutWindow
  -- showUserGuide

  setNextWindowPos pos ImGuiCond.Once Nothing
  setNextWindowSize size' ImGuiCond.Once
  -- Works, but will make the window contents illegible without doing something more involved.
  -- setNextWindowContentSize size'
  -- setNextWindowSizeConstraints size' size'
  setNextWindowCollapsed False ImGuiCond.Once

  setNextWindowBgAlpha 0.42

  begin "My Window"

  text "Hello!"

  beginTabBar "My tab bar" ImGuiTabBarFlags.Reorderable >>= whenTrue do
    beginTabItem "Tab 1" tab1Ref 0 >>= whenTrue do
      text "Tab 1 is currently selected."
      endTabItem
    beginTabItem "Tab 2" tab2Ref 0 >>= whenTrue do
      text "Tab 2 is selected now."
      endTabItem
    reOpen <- tabItemButton "ReopenTabs" ImGuiTabItemFlags.Trailing
    when reOpen do
      writeIORef tab1Ref True
      writeIORef tab2Ref True
    endTabBar

  listBox "Items" r [ "A", "B", "C" ]

  button "Click me" >>= \case
    True  -> openPopup "Button Popup"
    False -> return ()

  isItemHovered >>= whenTrue do
    beginTooltip
    text "Tooltip?"
    endTooltip

  beginPopup "Button Popup" >>= whenTrue do
    button "Close" >>= whenTrue closeCurrentPopup
    endPopup

  sameLine >> smallButton "Click me" >>= \case
    True  -> putStrLn "Oh hi Mark"
    False -> return ()

  sameLine >> arrowButton "Arrow" ImGuiDir.Up

  sameLine >> checkbox "Check!" checked >>= \case
    True  -> readIORef checked >>= print
    False -> return ()

  separator

  dragFloat3 "Slider" slider 0.1 0.0 1.0

  progressBar 0.314 (Just "Pi")

  beginChild "Child" (ImVec2 0 0) True 0

  beginCombo "Label" "Preview" >>= whenTrue do
    selectable "Testing 1"
    selectable "Testing 2"
    endCombo

  combo "Simple" selected [ "1", "2", "3" ]

  endChild

  text "ListClipper"
  withChildOpen "##fixed" (ImVec2 0 200) True 0 do
    let lotsOfItems = Vector.generate 50 (pack . mappend "Item " . show)
    withListClipper Nothing lotsOfItems text

  text "ListClipper, Haskell-powered"
  withChildOpen "##infinite" (ImVec2 0 200) True 0 do
    let infiniteItems = map (pack . mappend "Item " . show) [0 :: Int ..]
    withListClipper Nothing infiniteItems text

  text "Ethereal ListClipper"
  withChildOpen "##ethereal" (ImVec2 0 200) True 0 do
    withListClipper Nothing (ClipRange (0 :: Int) 1000) $
      text . pack . mappend "Item " . show

  plotHistogram "A histogram" [ 10, 10, 20, 30, 90 ]

  colorPicker3 "Test" color

  treeNode "Tree Node 1" >>= whenTrue do
    treeNode "Tree Node 2" >>= whenTrue do
      treePop

    treeNode "Tree Node 3" >>= whenTrue do
      treePop

    treePop

  beginMainMenuBar >>= whenTrue do
    beginMenu "Hello" >>= whenTrue do
      menuItem "Hello"
      endMenu

    beginMenu "World" >>= whenTrue do
      menuItem "World"
      endMenu

    endMainMenuBar

  end

  render

  glClear GL_COLOR_BUFFER_BIT
  ImplGL3.renderDrawData =<< getDrawData

  glSwapWindow window

  if shouldQuit
    then return ()
    else loop window checked color slider r pos size' selected tab1Ref tab2Ref

  where

    checkEvents = do
      ev <- ImplSDL2.pollEvent

      case ev of
        Nothing -> return False
        Just Event{ eventPayload } -> do
          let isQuit = case eventPayload of
                QuitEvent -> True
                _         -> False

          (isQuit ||) <$> checkEvents


whenTrue :: IO () -> Bool -> IO ()
whenTrue io True  = io
whenTrue _io False = return ()
