{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module: DearImGui.FontAtlas

Font atlas builder, accompanied with lower-level functions.

Since imgui 1.92, specifying glyph ranges is no longer necessary with modern
backends — glyphs are loaded on demand.

@
import qualified DearImGui.FontAtlas as FontAtlas

prepareAtlas =
  FontAtlas.rebuild
    [ FontAtlas.FromTTF "comic-sans-mono.ttf" (Just 13) (Just csOptions)
    , FontAtlas.DefaultFont
    ]
  where
    csOptions = mconcat
      [ FontAtlas.fontNo 1
      , FontAtlas.glyphOffset (0, -1)
      ]
@

-}

module DearImGui.FontAtlas
  ( -- * Main types
    Font
  , FontSource(..)
    -- * Building atlas
  , rebuild
    -- ** Configuring sources
  , ConfigSetup(..)
  , fontDataOwnedByAtlas
  , fontNo
  , sizePixels
  , oversampleH
  , oversampleV
  , pixelSnapH
  , glyphOffset
  , glyphExcludeRanges
  , glyphExtraAdvanceX
  , glyphMinAdvanceX
  , glyphMaxAdvanceX
  , mergeMode
  , fontLoaderFlags
  , rasterizerMultiply
  , ellipsisChar

    -- * Lower level types and functions
  , clear
  , setupFont
  , withConfig
  , addFontFromFileTTF
  , addFontFromFileTTF_
  )
  where

-- base
import Control.Exception (finally)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- managed
import Control.Monad.Managed
  ( MonadManaged, managed )
import qualified Control.Monad.Managed as Managed

-- unlift
import UnliftIO (MonadUnliftIO(..))

-- dear-imgui
import DearImGui.Structs (Font, FontConfig, ImFontAtlas, ImVec2(..), ImWchar)

-- dear-imgui-raw
import qualified DearImGui.Raw.ImFontAtlas as ImFontAtlas
import qualified DearImGui.Raw.ImFontConfig as ImFontConfig
import qualified DearImGui.Raw.ImGui as ImGui

-- | Font setup data
data FontSource
  = DefaultFont
  | FromTTF FilePath (Maybe Float) (Maybe ConfigSetup)
  -- TODO: FromMemory

-- | Font config monoid interface to be used in 'FontSource'.
--
-- @
-- mergeMode True <> fontNo 1
-- @
newtype ConfigSetup = ConfigSetup
  { applyToConfig :: FontConfig -> IO ()
  }

instance Semigroup ConfigSetup where
  ConfigSetup f <> ConfigSetup g =
    ConfigSetup \fc -> f fc >> g fc
instance Monoid ConfigSetup where
  mempty = ConfigSetup (const mempty)

-- | Rebuild font atlas with provided configuration
-- and return corresponding structure of font handles
-- to be used with 'DearImGui.withFont'.
--
-- Accepts any 'Traversable' instance, so you are free to use
-- lists, maps or custom structures.
{-# INLINE rebuild #-}
rebuild :: (MonadIO m, Traversable t) => t FontSource -> m (t Font)
rebuild sources = liftIO $ Managed.with action pure
  where
    action = do
      clear
      traverse setupFont sources

-- | Reset font atlas, clearing internal data
{-# INLINE clear #-}
clear :: (MonadIO m) => m ()
clear = liftIO do
  fontAtlas >>= ImFontAtlas.clear

fontAtlas :: IO (Ptr ImFontAtlas)
fontAtlas = do
  io <- ImGui.getIO
  peek io.fonts

-- | Load a font from TTF file.
--
-- Specify font path and optionally a size. Pass 'Nothing' to let imgui
-- pick the size automatically (recommended since 1.92).
--
-- Use 'DefaultFont' source if you want to retain built-in font too.
{-# INLINE addFontFromFileTTF #-}
addFontFromFileTTF :: MonadIO m
  => FilePath               -- ^ Font file path
  -> Maybe Float            -- ^ Font size in pixels (Nothing = automatic)
  -> Maybe FontConfig       -- ^ Configuration data
  -> m (Maybe Font)         -- ^ Returns font handle, if added successfully
addFontFromFileTTF font size config = liftIO do
  atlas <- fontAtlas
  ptr <- withCString font \fontPtr ->
    ImFontAtlas.addFontFromFileTTF
      atlas
      fontPtr
      (fromMaybe 0 size)
      (fromMaybe nullPtr config)
      nullPtr
  pure $
    if ptr == nullPtr
      then Nothing
      else Just ptr
      -- FIXME: turn off asserts, so it would work

{-# INLINE addFontFromFileTTF_ #-}
addFontFromFileTTF_ :: MonadIO m
  => FilePath       -- ^ Font file path
  -> m (Maybe Font) -- ^ Returns font handle, if added successfully
addFontFromFileTTF_ font =
  addFontFromFileTTF font Nothing Nothing

-- | Load a font with provided configuration, return its handle
-- and defer config destructors, if needed.
setupFont :: (MonadManaged m) => FontSource -> m Font
setupFont = \case
  DefaultFont -> liftIO do
    atlas <- fontAtlas
    ImFontAtlas.addFontDefault atlas nullPtr
  FromTTF path mbSize configSetup -> do
    config <- managed (withConfig configSetup)
    mFont <- addFontFromFileTTF path mbSize config
    case mFont of
      Nothing ->
        liftIO . fail $ "Couldn't load font from " <> path
      Just font ->
        pure font

-- | Configure font config with provided setup,
-- and execute a computation with built object.
{-# INLINE withConfig #-}
withConfig :: (MonadUnliftIO m) => Maybe ConfigSetup -> (Maybe FontConfig -> m a) -> m a
withConfig mSetup action =
  case mSetup of
    Nothing ->
      action Nothing
    Just (ConfigSetup setup) ->
      withRunInIO \run ->
        ImFontConfig.withDefault \config -> do
          setup config
          run (action (Just config)) `finally` freeGlyphExcludeRanges config

freeGlyphExcludeRanges :: FontConfig -> IO ()
freeGlyphExcludeRanges config = do
  ranges <- peek config.glyphExcludeRanges
  unless (ranges == nullPtr) (free ranges)

setField :: Storable a => (FontConfig -> Ptr a) -> (v -> a) -> v -> ConfigSetup
setField field convert value =
  ConfigSetup \fc -> poke (field fc) (convert value)

-- | TTF/OTF data ownership taken by the container ImFontAtlas (will delete memory itself).
--
-- By default, it is @true@
fontDataOwnedByAtlas :: Bool -> ConfigSetup
fontDataOwnedByAtlas = setField (.fontDataOwnedByAtlas) fromBool

-- | Index of font within TTF/OTF file.
--
-- By default, it is @0@
fontNo :: Int -> ConfigSetup
fontNo = setField (.fontNo) fromIntegral

-- | Size in pixels for rasterizer
--
-- More or less maps to the resulting font height.
--
-- Implicitly set by @addFont...@ functions.
sizePixels :: Float -> ConfigSetup
sizePixels = setField (.sizePixels) id

-- | Rasterize at higher quality for sub-pixel positioning.
--
-- Note: the difference between 2 and 3 is minimal so you can reduce this to 2 to save memory.
-- Read https://github.com/nothings/stb/blob/master/tests/oversample/README.md for details.
--
-- By default, it is @3@
oversampleH :: Int -> ConfigSetup
oversampleH = setField (.oversampleH) fromIntegral

-- | Rasterize at higher quality for sub-pixel positioning.
--
-- This is not really useful as we don't use sub-pixel positions on the Y axis.
--
-- By default, it is @1@
oversampleV :: Int -> ConfigSetup
oversampleV = setField (.oversampleV) fromIntegral

-- | Align every glyph to pixel boundary.
--
-- Useful if you are merging a non-pixel aligned font with the default font.
-- If enabled, you can set OversampleH/V to 1.
--
-- By default, it is @false@
pixelSnapH :: Bool -> ConfigSetup
pixelSnapH = setField (.pixelSnapH) fromBool

-- | Offset all glyphs from this font input.
--
-- By default, it is @0, 0@
glyphOffset :: (Float, Float) -> ConfigSetup
glyphOffset = setField (.glyphOffset) (uncurry ImVec2)

-- | Exclude specific Unicode ranges from this font source.
-- Need in case you might have undesirable overlapping ranges by merging fonts.
-- See https://github.com/ocornut/imgui/blob/master/docs/FONTS.md#excluding-overlapping-ranges
--
-- Example:
--
-- @
-- -- Exclude A-Z
-- glyphExcludeRanges [(fromEnum 'A', fromEnum 'Z')]
-- @
glyphExcludeRanges :: [(ImWchar, ImWchar)] -> ConfigSetup
glyphExcludeRanges pairs =
  ConfigSetup \fc -> do
    ptr <- newArray ([x | (start, end) <- pairs, x <- [start, end]] <> [0])
    poke fc.glyphExcludeRanges ptr

-- | Extra spacing (in pixels) between glyphs.
--
-- By default, it is @0@
glyphExtraAdvanceX :: Float -> ConfigSetup
glyphExtraAdvanceX = setField (.glyphExtraAdvanceX) id

-- | Minimum AdvanceX for glyphs.
--
-- Set Min to align font icons, set both Min/Max to enforce mono-space font.
--
-- By default, it is @0@
glyphMinAdvanceX :: Float -> ConfigSetup
glyphMinAdvanceX = setField (.glyphMinAdvanceX) id

-- | Maximum AdvanceX for glyphs.
--
-- By default, it is @FLT_MAX@.
glyphMaxAdvanceX :: Float -> ConfigSetup
glyphMaxAdvanceX = setField (.glyphMaxAdvanceX) id

-- | Merge into previous ImFont, so you can combine multiple inputs font into one ImFont.
--
-- e.g. ASCII font + icons + Japanese glyphs.
-- You may want to use @GlyphOffset.y@ when merging font of different heights.
--
-- By default, it is @false@
mergeMode :: Bool -> ConfigSetup
mergeMode = setField (.mergeMode) fromBool

-- | Settings for custom font loader.
--
-- THIS IS LOADER IMPLEMENTATION DEPENDENT.
--
-- By default, it is @0@. Leave it so if unsure.
fontLoaderFlags :: Int -> ConfigSetup
fontLoaderFlags = setField (.fontLoaderFlags) fromIntegral

-- | Brighten (>1.0f) or darken (<1.0f) font output.
--
-- Brightening small fonts may be a good workaround to make them more readable.
--
-- By default, it is @1.0f@.
rasterizerMultiply :: Float -> ConfigSetup
rasterizerMultiply = setField (.rasterizerMultiply) id

-- | Explicitly specify unicode codepoint of ellipsis character.
--
-- When fonts are being merged first specified ellipsis will be used.
--
-- By default, it is @-1@
ellipsisChar :: ImWchar -> ConfigSetup
ellipsisChar = setField (.ellipsisChar) id
