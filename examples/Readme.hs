-- NOTE: If this is file is edited, please also copy and paste it into
-- README.md.

{-# language OverloadedStrings #-}

module Main ( main ) where

import DearImGui
import qualified DearImGui.Impl.OpenGL3 as ImplGL3
import qualified DearImGui.Impl.SDL2 as ImplSDL2

import Graphics.GL
import SDL

import Control.Monad.Managed
import Control.Monad.IO.Class ()
import Control.Monad (when, unless)
import Control.Exception (bracket)

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged $ do
    -- Create a window using SDL; as we're using OpenGL, we enable OpenGL too
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    managed_ $ ImplSDL2.withInitForOpenGL window glContext
    -- Initialize ImGui's OpenGL backend
    managed_ $ ImplGL3.withInit Nothing

    liftIO $ mainLoop window

mainLoop :: Window -> IO ()
mainLoop window = unlessQuit $ do
  -- Tell ImGui we're starting a new frame
  ImplGL3.newFrame
  ImplSDL2.newFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" $ do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \clicked ->
      when clicked $ putStrLn "Ow!"

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT
  render
  ImplGL3.renderDrawData =<< getDrawData

  glSwapWindow window
  mainLoop window
  where
  -- Process the event loop
  unlessQuit action = do
    shouldQuit <- gotQuitEvent
    unless shouldQuit action

  gotQuitEvent = do
    ev <- ImplSDL2.pollEvent

    case ev of
      Nothing ->
        return False
      Just event ->
        (isQuit event ||) <$> gotQuitEvent

  isQuit event =
    eventPayload event == QuitEvent
