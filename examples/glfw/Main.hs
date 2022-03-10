{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Main ( main ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.Bits ((.|.))
import Data.IORef
import Data.List (sortBy)
import Data.Foldable (traverse_)
import Data.Text (Text, pack)

import DearImGui
import DearImGui.OpenGL2
import DearImGui.GLFW
import DearImGui.GLFW.OpenGL
import Graphics.GL
import Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  initialised <- GLFW.init
  unless initialised $ error "GLFW init failed"

  runManaged $ do
    mwin <- managed $ bracket
      (GLFW.createWindow 800 600 "Hello, Dear ImGui!" Nothing Nothing)
      (maybe (return ()) GLFW.destroyWindow)
    case mwin of
      Just win -> do
        liftIO $ do
          GLFW.makeContextCurrent (Just win)
          GLFW.swapInterval 1

        -- Create an ImGui context
        _ <- managed $ bracket createContext destroyContext

        -- Initialize ImGui's GLFW backend
        _ <- managed_ $ bracket_ (glfwInitForOpenGL win True) glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

        tableRef <- liftIO $ newIORef
          [ (1,  "foo")
          , (2,  "bar")
          , (3,  "baz")
          , (10, "spam")
          , (11, "spam")
          , (12, "spam")
          ]

        liftIO $ mainLoop win tableRef
      Nothing -> do
        error "GLFW createWindow failed"

  GLFW.terminate

mainLoop :: Window -> IORef [(Integer, Text)] -> IO ()
mainLoop win tableRef = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose win
  unless close do

    -- Tell ImGui we're starting a new frame
    openGL2NewFrame
    glfwNewFrame
    newFrame

    -- Build the GUI
    bracket_ (begin "Hello, ImGui!") end do
      -- Add a text widget
      text "Hello, ImGui!"

      -- Add a button widget, and call 'putStrLn' when it's clicked
      clicking <- button "Clickety Click"
      when clicking $
        putStrLn "Ow!"
      itemContextPopup do
        text "pop!"
        button "ok" >>= \clicked ->
          when clicked $
            closeCurrentPopup

      newLine

      mkTable tableRef

    -- Render
    glClear GL_COLOR_BUFFER_BIT

    render
    openGL2RenderDrawData =<< getDrawData

    GLFW.swapBuffers win

    mainLoop win tableRef

mkTable :: IORef [(Integer, Text)] -> IO ()
mkTable tableRef =
  withTableOpen sortable "MyTable" 3 $ do
    tableSetupColumn "Hello"
    tableSetupColumnWith defTableColumnOptions "World"

    withSortableTable \isDirty sortSpecs ->
      when (isDirty && not (null sortSpecs)) do
        -- XXX: do your sorting & cache it. Dont sort every frame.
        putStrLn "So dirty!"
        print sortSpecs
        modifyIORef' tableRef . sortBy $
          foldMap mkCompare sortSpecs

    tableHeadersRow
    readIORef tableRef >>=
      traverse_ \(ix, title) -> do
        tableNextRow
        tableNextColumn $ text (pack $ show ix)
        tableNextColumn $ text title
        tableNextColumn $ void (button "♥")
  where
    mkCompare TableSortingSpecs{..} a b =
      let
        dir = if tableSortingReverse then flip else id
      in
        case tableSortingColumn of
          0 -> dir compare (fst a) (fst b)
          1 -> dir compare (snd a) (snd b)
          _ -> EQ

    sortable = defTableOptions
      { tableFlags =
          ImGuiTableFlags_Sortable .|.
          ImGuiTableFlags_SortMulti
      }
