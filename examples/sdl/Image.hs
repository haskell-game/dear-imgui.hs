{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import qualified DearImGui.Raw as Raw
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import qualified SDL as SDL

--  For the texture creation
import Foreign
import Foreign.Ptr
import qualified Numeric.LinearAlgebra as M
import Foreign.Marshal.Alloc
import Data.IORef
import qualified Data.Vector.Storable as VS

data Texture = Texture {textureID :: GLuint, textureWidth :: GLsizei, textureHeight :: GLsizei}

-- |Creates a texture in memory
--
-- Reserves space on the texture-memory for width*height
create2DTexture :: Int -> Int -> IO Texture
create2DTexture width height = do
  alloca $ \ptr -> do
    glGenTextures 1 ptr
    tID <- peek ptr
    return Texture {textureID = tID, textureWidth = fromIntegral width, textureHeight = fromIntegral height}
    

--createDummyTexture :: IORef Texture -> M.Matrix (Float) -> IO ()
--createDummyTexture texture matrix = do
createDummyTexture :: Texture -> IO ()
createDummyTexture texture = do
  let width = textureWidth texture
  let height = textureWidth texture
  glEnable GL_TEXTURE_2D
  glBindTexture GL_TEXTURE_2D $ textureID texture
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  let dat = VS.generate (3*(fromIntegral width)*(fromIntegral height)) (\i -> fromIntegral $ 17 + (50 * (i `mod` 3)):: GLubyte) :: VS.Vector GLubyte -- some blueish
  VS.unsafeWith dat $ \dataPtr -> do
    glTexImage2D GL_TEXTURE_2D 0 GL_RGB width height 0 GL_RGB GL_UNSIGNED_BYTE (castPtr dataPtr)
    return()
  glBindTexture GL_TEXTURE_2D 0
  return ()


gui :: IO ()
gui = do


  -- Initialize SDL
  SDL.initializeAll



  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    w <- do
      let title = "Hello, Dear ImGui!"
      let config = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL, SDL.windowResizable = True }
      managed $ bracket (SDL.createWindow title config) SDL.destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (SDL.glCreateContext w) SDL.glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL w glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init (do 
      putStrLn "ImguiOpenGL shut down"
      openGL3Shutdown)


    liftIO $ do 
      txt <- create2DTexture 500 500 
      createDummyTexture txt
      err <- glGetError
      putStrLn $  "Error-code: " ++ show err
     
    liftIO $ do
      mainLoop w 1 -- 1 is actually the Ptr address


mainLoop :: SDL.Window ->  GLuint -> IO ()
mainLoop w c = do
  -- Process the event loop
  untilNothingM pollEventWithImGui

  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  bracket_ (begin "GL") end $ do
--    image (intPtrToPtr $ fromIntegral c) (ImVec2 500 500)(ImVec2 0 0)(ImVec2 1 1)(ImVec4 1 1 1 1)(ImVec4 0 0 0 0)
    Foreign.with (ImVec2 500 500) \sizePtr ->
      Foreign.with (ImVec2 0 0) \uv0Ptr -> 
        Foreign.with (ImVec2 1 1) \uv1Ptr -> 
          Foreign.with (ImVec4 1 1 1 1) \tintColPtr -> 
            Foreign.with (ImVec4 0 0 0 0) \borderColPtr -> do
              Raw.image (intPtrToPtr $ fromIntegral c) sizePtr uv0Ptr uv1Ptr tintColPtr borderColPtr

  -- Render
  render
  glClear GL_COLOR_BUFFER_BIT

  openGL3RenderDrawData =<< getDrawData

  SDL.glSwapWindow w

  mainLoop w c

  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)

main = do gui
