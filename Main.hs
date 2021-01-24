{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
import DearImGui
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
      openGL2Init

      newIORef False >>= loop w

      openGL2Shutdown

loop :: Window -> IORef Bool -> IO ()
loop w checked = do
  ev <- pollEventWithImGui

  openGL2NewFrame
  sdl2NewFrame w
  newFrame

  -- showDemoWindow
  -- showMetricsWindow
  -- showAboutWindow
  -- showUserGuide

  begin "My Window"
  text "Hello!"

  button "Click me" >>= \case
    True  -> putStrLn "Oh hi Mark"
    False -> return ()

  smallButton "Click me" >>= \case
    True  -> putStrLn "Oh hi Mark"
    False -> return ()

  arrowButton "Arrow" ImGuiDirUp

  checkbox "Check!" checked >>= \case
    True  -> readIORef checked >>= print
    False -> return ()

  progressBar 0.314 (Just "Pi")

  beginCombo "Label" "Preview" >>= whenTrue do
    selectable "Testing 1"
    selectable "Testing 2"
    endCombo

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

  case ev of
    Nothing -> loop w checked
    Just Event{ eventPayload } -> case eventPayload of
      QuitEvent -> return ()
      _         -> loop w checked


whenTrue :: IO () -> Bool -> IO ()
whenTrue io True  = io
whenTrue io False = return ()
