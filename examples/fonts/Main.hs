{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

{- | Font usage example.

Loads two non-standard fonts

This example uses NotoSansJP-Regular.otf from Google Fonts
Licensed under the SIL Open Font License, Version 1.1
https://fonts.google.com/noto/specimen/Noto+Sans+JP
-}

module Main ( main ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.IORef
import DearImGui
import DearImGui.Fonts
import DearImGui.OpenGL2
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL

main :: IO ()
main = do
  initializeAll

  runManaged do
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    _ <- managed $ bracket createContext destroyContext
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

    -- The first loaded font is set as a global default.
    addFontFromFileTTF' "./imgui/misc/fonts/DroidSans.ttf" 12 
                                      Nothing (Just getGlyphRangesCyrillic) >>= \case
      Nothing -> error "couldn't load DroidSans.ttf"
      Just font -> return font
    
    -- You also may use a default hardcoded font for some purposes (i.e. as fallback)
    defaultFont <- addFontDefault

    notoFont <- addFontFromFileTTF' "./examples/fonts/NotoSansJP-Regular.otf" 16 
                                     Nothing (Just getGlyphRangesJapanese) >>= \case
      Nothing -> error "couldn't load NotoSansJP-Regular.otf"
      Just font -> return font
    
    -- If you build fonts from memory or use glyphs, you should explicitly
    -- build font atlas with buildAtlas. Though, it's not needed here.

    liftIO $ do
      fontFlag <- newIORef False
      mainLoop window defaultFont notoFont fontFlag


mainLoop :: Window -> Font -> Font -> IORef Bool -> IO ()
mainLoop window defaultFont notoFont fontFlag = loop 
  where 
  loop = unlessQuit do
    openGL2NewFrame
    sdl2NewFrame
    newFrame

    withWindowOpen "Hello, ImGui!" do
      -- To use a font for widget text, you may either put it
      -- into a 'withFont' block:
      withFont defaultFont do 
        text "Hello, ImGui!"
      text "Привет, ImGui!"
      
      -- ...or you can explicitly push and pop a font.
      -- Though it's not recommended.
      flagValue <- readIORef fontFlag
      let buttonText = 
            if flagValue then "私をクリックしてください"
                         else "Click Me!"
      if flagValue then do          
        pushFont notoFont
        text "こんにちは, ImGui!"
      else do 
        text "Hola, ImGui!"
      button buttonText >>= \case 
        True -> modifyIORef' fontFlag (not)        
        False -> return ()
      when flagValue do popFont

    showDemoWindow

    glClear GL_COLOR_BUFFER_BIT
    render
    openGL2RenderDrawData =<< getDrawData
    glSwapWindow window

    loop

  unlessQuit action = do
    shouldQuit <- checkEvents
    if shouldQuit then pure () else action

  checkEvents = do
    pollEventWithImGui >>= \case
      Nothing ->
        return False
      Just event ->
        (isQuit event ||) <$> checkEvents

  isQuit event =
    SDL.eventPayload event == SDL.QuitEvent
