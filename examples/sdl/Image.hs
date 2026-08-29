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
import qualified DearImGui.Raw.Enums.ImDrawFlags as ImDrawFlags
import qualified DearImGui.Raw.ImDrawList as ImDrawList
import qualified DearImGui.Impl.OpenGL3 as ImplGL3
import qualified DearImGui.Impl.SDL2 as ImplSDL2
import Graphics.GL
import qualified SDL as SDL

--  For the texture creation
import Data.Fixed (mod')
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

type RGB = (GLubyte, GLubyte, GLubyte)

fill :: Texture -> (Double -> Double -> RGB) -> VS.Vector GLubyte
fill texture pixel =
  VS.fromListN (3 * width * height)
    [ channel
    | py <- [0 .. height - 1]
    , px <- [0 .. width - 1]
    , let (r, g, b) = pixel (fromIntegral px / fromIntegral width) (fromIntegral py / fromIntegral height)
    , channel <- [r, g, b]
    ]
  where
    width  = fromIntegral (textureWidth texture)
    height = fromIntegral (textureHeight texture)

plasma :: Double -> Double -> RGB
plasma u v = (sinePalette 0, sinePalette 2, sinePalette 4)
  where
    sinePalette phase = round (128 + 127 * sin (interference + phase))
    interference =
      sin (10 * u)
        + sin (8 * v + 1)
        + sin (6 * (u + v))
        + sin (20 * sqrt ((u - 0.5) ^ (2 :: Int) + (v - 0.5) ^ (2 :: Int)))

colourWheel :: Double -> Double -> Double -> RGB
colourWheel aspect u v
  | radius <= 1 = hsvToRgb hue radius 1
  | even (floor (u * 12) + floor (v * 16) :: Int) = (0x30, 0x30, 0x30)
  | otherwise = (0x50, 0x50, 0x50)
  where
    dx = (u - 0.5) * 2
    dy = (v - 0.5) * 2 / aspect
    radius = sqrt (dx * dx + dy * dy)
    hue = atan2 dy dx / (2 * pi) + 0.5

hsvToRgb :: Double -> Double -> Double -> RGB
hsvToRgb h s v = (byte (r + m), byte (g + m), byte (b + m))
  where
    chroma = v * s
    sector = h * 6
    secondary = chroma * (1 - abs (sector `mod'` 2 - 1))
    m = v - chroma
    byte = round . (* 255)
    (r, g, b) = case floor sector :: Int of
      0 -> (chroma, secondary, 0)
      1 -> (secondary, chroma, 0)
      2 -> (0, chroma, secondary)
      3 -> (0, secondary, chroma)
      4 -> (secondary, 0, chroma)
      _ -> (chroma, 0, secondary)


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
    managed_ $ ImplSDL2.withInitForOpenGL window glContext

    -- Initialize ImGui's OpenGL backend
    managed_ $ ImplGL3.withInit Nothing

    liftIO do
      plasmaTexture <- create2DTexture 320 240
      VS.unsafeWith (fill plasmaTexture plasma) $
        bindTexture plasmaTexture

      wheelTexture <- create2DTexture 240 320
      VS.unsafeWith (fill wheelTexture (colourWheel (240 / 320))) $
        bindTexture wheelTexture

      err <- glGetError
      putStrLn $ "Error-code: " ++ show err

      print (plasmaTexture, wheelTexture)
      mainLoop window (plasmaTexture, wheelTexture) False

mainLoop :: SDL.Window -> (Texture, Texture) -> Bool -> IO ()
mainLoop window textures flag = unlessQuit do
  -- Tell ImGui we're starting a new frame
  ImplGL3.newFrame
  ImplSDL2.newFrame
  newFrame

  let texture = if flag then fst textures else snd textures
  -- Drawing images require some backend-specific code.
  -- Meanwhile, we have to deal with raw bindings.
  let openGLtextureID = fromIntegral $ textureID texture

  -- Build the GUI
  clicked <- withWindow "Image example" \open ->
    if open then do
      text "That's an image, click it"
      newLine

      -- Using imageButton
      imageButton
        "##btn"
        (textureRefFromID openGLtextureID)
        (textureSize texture)
        (ImVec2 0 0)
        (ImVec2 1 1)
        (ImVec4 1 1 1 1)
        (ImVec4 1 1 1 1)
    else
      pure False

  -- Using DrawList
  bg <- getBackgroundDrawList
  ImDrawList.addImageRounded
    bg
    (textureRefFromID openGLtextureID)
    (ImVec2 100 100) (ImVec2 200 200)
    (ImVec2 0.25 0.25) (ImVec2 0.75 0.75)
    (imCol32 0 255 0 0xFF) -- Extract green channel
    32 ImDrawFlags.RoundCornersBottom

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  DearImGui.render
  DearImGui.getDrawData >>= ImplGL3.renderDrawData

  SDL.glSwapWindow window

  mainLoop window textures (flag /= clicked)

  where
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      ImplSDL2.pollEvent >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent
