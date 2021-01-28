{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
import DearImGui
import DearImGui.OpenGL
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
    bracket_ openGL2Init openGL2Shutdown do
      checkVersion
      styleColorsLight

      checked <- newIORef False
      color <- newIORef $ ImVec3 1 0 0
      slider <- newIORef (0.42, 0, 0.314)
      r <- newIORef 4
      loop w checked color slider r

      openGL2Shutdown


loop :: Window -> IORef Bool -> IORef ImVec3 -> IORef (Float, Float, Float) -> IORef Int -> IO ()
loop w checked color slider r = do
  quit <- pollEvents

  openGL2NewFrame
  sdl2NewFrame w
  newFrame

  -- showDemoWindow
  -- showMetricsWindow
  -- showAboutWindow
  -- showUserGuide

  begin "My Window"
  text "Hello!"

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
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow w

  if quit then return () else loop w checked color slider r

  where

    pollEvents = do
      ev <- pollEventWithImGui

      case ev of
        Nothing -> return False
        Just Event{ eventPayload } -> do
          let isQuit = case eventPayload of
                QuitEvent -> True
                _         -> False

          (isQuit ||) <$> pollEvents


whenTrue :: IO () -> Bool -> IO ()
whenTrue io True  = io
whenTrue io False = return ()
