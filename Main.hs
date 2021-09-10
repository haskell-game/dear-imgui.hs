{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.IORef
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Control.Exception
import Graphics.GL
import SDL

main :: IO ()
main = do
  initializeAll

  bracket (createWindow "Hello, Dear ImGui!" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }) destroyWindow \w ->
    bracket (glCreateContext w) glDeleteContext \glContext ->
    bracket createContext destroyContext \_imguiContext ->
    bracket_ (sdl2InitForOpenGL w glContext) sdl2Shutdown $
    bracket_ openGL3Init openGL3Shutdown do
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

  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- showDemoWindow
  -- showMetricsWindow
  -- showAboutWindow
  -- showUserGuide

  setNextWindowPos pos ImGuiCond_Once Nothing
  setNextWindowSize size' ImGuiCond_Once
  -- Works, but will make the window contents illegible without doing something more involved.
  -- setNextWindowContentSize size'
  -- setNextWindowSizeConstraints size' size'
  setNextWindowCollapsed False ImGuiCond_Once

  setNextWindowBgAlpha 0.42

  begin "My Window"

  text "Hello!"

  beginTabBar "My tab bar" ImGuiTabBarFlags_Reorderable >>= whenTrue do
    beginTabItem "Tab 1" tab1Ref ImGuiTabBarFlags_None >>= whenTrue do
      text "Tab 1 is currently selected."
      endTabItem
    beginTabItem "Tab 2" tab2Ref ImGuiTabBarFlags_None >>= whenTrue do
      text "Tab 2 is selected now."
      endTabItem
    reOpen <- tabItemButton "ReopenTabs" ImGuiTabItemFlags_Trailing
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

  sameLine >> arrowButton "Arrow" ImGuiDir_Up

  sameLine >> checkbox "Check!" checked >>= \case
    True  -> readIORef checked >>= print
    False -> return ()

  separator

  dragFloat3 "Slider" slider 0.1 0.0 1.0

  progressBar 0.314 (Just "Pi")

  beginChild "Child"

  beginCombo "Label" "Preview" >>= whenTrue do
    selectable "Testing 1"
    selectable "Testing 2"
    endCombo

  combo "Simple" selected [ "1", "2", "3" ]

  endChild

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
  openGL3RenderDrawData =<< getDrawData

  glSwapWindow window

  if shouldQuit
    then return ()
    else loop window checked color slider r pos size' selected tab1Ref tab2Ref

  where

    checkEvents = do
      ev <- pollEventWithImGui

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
