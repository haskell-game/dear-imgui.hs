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
import qualified DearImGui.Raw.DrawList as DrawList
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
  deriving (Show)

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

fill :: Texture -> (GLubyte, GLubyte, GLubyte) -> VS.Vector GLubyte
fill texture (r, g, b) =
  VS.generate
    (3 * width * height)
    (\i ->
        case i `mod` 3 of
          0 -> r
          1 -> g
          2 -> b
          _ -> error "assert: 3-byte pitch"
    )
  where
    width  = fromIntegral (textureWidth texture)
    height = fromIntegral (textureHeight texture)


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
      blueish <- create2DTexture 320 240
      VS.unsafeWith (fill blueish (0x00, 0x7F, 0xFF)) $
        bindTexture blueish

      pinkish <- create2DTexture 240 320
      VS.unsafeWith (fill pinkish (0xFF, 0x00, 0x7F)) $
        bindTexture pinkish

      err <- glGetError
      putStrLn $ "Error-code: " ++ show err

      print (blueish, pinkish)
      mainLoop window (blueish, pinkish) False

mainLoop :: SDL.Window -> (Texture, Texture) -> Bool -> IO ()
mainLoop window textures flag = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  let texture = if flag then fst textures else snd textures
  -- Drawing images require some backend-specific code.
  -- Meanwhile, we have to deal with raw bindings.
  let openGLtextureID = intPtrToPtr $ fromIntegral $ textureID texture

  -- Build the GUI
  clicked <- withWindow "Image example" \open ->
    if open then do
      text "That's an image, click it"
      newLine

      -- Using imageButton
      Foreign.with (textureSize texture) \sizePtr ->
        Foreign.with (ImVec2 0 0) \uv0Ptr ->
          Foreign.with (ImVec2 1 1) \uv1Ptr ->
            Foreign.with (ImVec4 1 1 1 1) \tintColPtr ->
              Foreign.with (ImVec4 1 1 1 1) \bgColPtr ->
                Raw.imageButton openGLtextureID sizePtr uv0Ptr uv1Ptr (-1) bgColPtr tintColPtr
    else
      pure False

  -- Using DrawList
  bg <- getBackgroundDrawList
  Foreign.with (ImVec2 100 100) \pMin ->
    Foreign.with (ImVec2 200 200) \pMax ->
      Foreign.with (ImVec2 0.25 0.25) \uvMin ->
        Foreign.with (ImVec2 0.75 0.75) \uvMax ->
          DrawList.addImageRounded
            bg
            openGLtextureID
            pMin pMax uvMin uvMax
            (Raw.imCol32 0 255 0 0xFF) -- Extract green channel
            32 ImDrawFlags_RoundCornersBottom

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  DearImGui.render
  DearImGui.getDrawData >>= openGL3RenderDrawData

  SDL.glSwapWindow window

  mainLoop window textures (flag /= clicked)

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
