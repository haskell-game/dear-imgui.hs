{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

-- | Port of [example_sdl2_sdlrenderer2](https://github.com/ocornut/imgui/blob/54c1bdecebf3c9bb9259c07c5f5666bb4bd5c3ea/examples/example_sdl2_sdlrenderer2/main.cpp).
--
-- Minor differences:
-- - No changing of the clear color via @ImGui::ColorEdit3@ as a Haskell binding
-- doesn't yet exist for this function.
-- - No high DPI renderer scaling as this seems to be in flux [upstream](https://github.com/ocornut/imgui/issues/6065)

module Main ( main ) where

import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Managed (managed, managed_, runManaged)
import Data.IORef (IORef, newIORef)
import Data.Text (pack)
import DearImGui
import DearImGui.SDL (pollEventWithImGui, sdl2NewFrame, sdl2Shutdown)
import DearImGui.SDL.Renderer
  ( sdl2InitForSDLRenderer, sdlRendererInit, sdlRendererNewFrame, sdlRendererRenderDrawData
  , sdlRendererShutdown
  )
import SDL (V4(V4), ($=), ($~), get)
import Text.Printf (printf)
import qualified SDL


main :: IO ()
main = do
  -- Initialize SDL2
  SDL.initializeAll

  runManaged do
    -- Create a window using SDL2
    window <- do
      let title = "ImGui + SDL2 Renderer"
      let config = SDL.defaultWindow
            { SDL.windowInitialSize = SDL.V2 1280 720
            , SDL.windowResizable = True
            , SDL.windowPosition = SDL.Centered
            }
      managed $ bracket (SDL.createWindow title config) SDL.destroyWindow

    -- Create an SDL2 renderer
    renderer <- managed do
      bracket
        (SDL.createRenderer window (-1) SDL.defaultRenderer)
        SDL.destroyRenderer

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ do
      bracket_ (sdl2InitForSDLRenderer window renderer) sdl2Shutdown

    -- Initialize ImGui's SDL2 renderer backend
    _ <- managed_ $ bracket_ (sdlRendererInit renderer) sdlRendererShutdown

    liftIO $ mainLoop renderer


mainLoop :: SDL.Renderer -> IO ()
mainLoop renderer = do
  refs <- newRefs
  go refs
  where
  go refs = unlessQuit do
    -- Tell ImGui we're starting a new frame
    sdlRendererNewFrame
    sdl2NewFrame
    newFrame

    -- Show the ImGui demo window
    get (refsShowDemoWindow refs) >>= \case
      False -> pure ()
      True -> showDemoWindow

    withWindowOpen "Hello, world!" do
      text "This is some useful text."
      _ <- checkbox "Demo Window" $ refsShowDemoWindow refs
      _ <- checkbox "Another Window" $ refsShowAnotherWindow refs
      _ <- sliderFloat "float" (refsFloat refs) 0 1

      button "Button" >>= \case
        False -> pure ()
        True -> refsCounter refs $~ succ
      sameLine
      counter <- get $ refsCounter refs
      text $ "counter = " <> pack (show counter)

      fr <- framerate
      text
        $ pack
        $ printf "Application average %.3f ms/frame (%.1f FPS)" (1000 / fr) fr

    get (refsShowAnotherWindow refs) >>= \case
      False -> pure ()
      True ->
        withCloseableWindow "Another Window" (refsShowAnotherWindow refs) do
          text "Hello from another window!"
          button "Close Me" >>= \case
            False -> pure ()
            True -> refsShowAnotherWindow refs $= False

    -- Render
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    SDL.clear renderer
    render
    sdlRendererRenderDrawData =<< getDrawData
    SDL.present renderer

    go refs

  -- Process the event loop
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


data Refs = Refs
  { refsShowDemoWindow :: IORef Bool
  , refsShowAnotherWindow :: IORef Bool
  , refsFloat :: IORef Float
  , refsCounter :: IORef Int
  }

newRefs :: IO Refs
newRefs =
  Refs
    <$> newIORef True
    <*> newIORef False
    <*> newIORef 0
    <*> newIORef 0
