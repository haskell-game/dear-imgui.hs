{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

{- | Drawing an DearImGui image using OpenGL textures.

https://github.com/ocornut/imgui/wiki/Image-Loading-and-Displaying-Examples
-}

module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (managed, managed_, runManaged)
import DearImGui
import qualified DearImGui.Raw as Raw
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import qualified SDL as SDL

--  For the texture creation
import Foreign
import qualified Data.Vector.Storable as VS

data Texture = Texture
  { textureID     :: GLuint
  , textureWidth  :: GLsizei
  , textureHeight :: GLsizei
  }

textureSize :: Texture -> ImVec2
textureSize texture =
  ImVec2
    (fromIntegral $ textureWidth texture)
    (fromIntegral $ textureHeight texture)

-- | Create a texture pointer in GL memory.
create2DTexture :: Int -> Int -> IO Texture
create2DTexture width height =
  alloca \ptr -> do
    glGenTextures 1 ptr
    tID <- peek ptr
    return Texture
      { textureID     = tID
      , textureWidth  = fromIntegral width
      , textureHeight = fromIntegral height
      }

bindTexture :: Texture -> Ptr GLubyte -> IO ()
bindTexture texture dataPtr = do
  glEnable GL_TEXTURE_2D
  glBindTexture GL_TEXTURE_2D (textureID texture)

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT

  glTexImage2D
    GL_TEXTURE_2D
    0
    GL_RGB
    (textureWidth texture)
    (textureHeight texture)
    0
    GL_RGB
    GL_UNSIGNED_BYTE
    (castPtr dataPtr)

fill :: Integral size => size -> size -> VS.Vector GLubyte
fill width height =
  VS.generate
    (3 * fromIntegral width * fromIntegral height)
    (\i ->
        case i `mod` 3 of
          0 -> 0x00
          1 -> 0x7F
          2 -> 0xFF
          _ -> error "assert: 3-byte pitch"
    )


main :: IO ()
main = do
  -- Initialize SDL
  SDL.initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL, SDL.windowResizable = True }
      managed $ bracket (SDL.createWindow title config) SDL.destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (SDL.glCreateContext window) SDL.glDeleteContext

    -- Create an ImGui context
    _dearContext <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    managed_ $ bracket_ openGL3Init do
      putStrLn "ImguiOpenGL shut down"
      openGL3Shutdown

    liftIO do
      let width = 320
          height = 240
      texture <- create2DTexture width height
      VS.unsafeWith (fill width height) $
        bindTexture texture
      err <- glGetError
      putStrLn $ "Error-code: " ++ show err

      mainLoop window texture

mainLoop :: SDL.Window -> Texture -> IO ()
mainLoop window texture = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Image example" $ runManaged do
    -- Drawing images require some backend-specific code.
    -- Meanwhile, we have to deal with raw binding.
    let openGLtextureID = intPtrToPtr $ fromIntegral $ textureID texture
    sizePtr <- managed $ Foreign.with (textureSize texture)
    uv0Ptr <- managed $ Foreign.with (ImVec2 0 0)
    uv1Ptr <- managed $ Foreign.with (ImVec2 1 1)
    tintColPtr <- managed $ Foreign.with (ImVec4 1 1 1 1)
    borderColPtr <- managed $ Foreign.with (ImVec4 0 1 0 0)
    Raw.image openGLtextureID sizePtr uv0Ptr uv1Ptr tintColPtr borderColPtr

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  DearImGui.render
  DearImGui.getDrawData >>= openGL3RenderDrawData

  SDL.glSwapWindow window

  mainLoop window texture

  where
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
