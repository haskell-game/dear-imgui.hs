{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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

      loop w

      openGL2Shutdown

loop :: Window -> IO ()
loop w = do
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

  end

  render

  glClear GL_COLOR_BUFFER_BIT
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow w

  case ev of
    Nothing -> loop w
    Just Event{ eventPayload } -> case eventPayload of
      QuitEvent -> return ()
      _         -> loop w
