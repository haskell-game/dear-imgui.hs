{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.FontAtlas

Font atlas builder, accompanied with lower-level functions.

@
import qualified DearImGui.FontAtlas as FontAtlas

prepareAtlas =
  FontAtlas.rebuild
    [ FontAtlas.FileTTF "comic-sans-mono.ttf" 13 csOptions csRanges
    , FontAtlas.Default
    ]
  where
    csOptions = mconcat
      [ FontAtlas.fontNo 1
      , FontAtlas.glyphOffset (0, -1)
      ]

    csRanges = RangeBuilder $ mconcat
      [ FontAtlas.addText "Hello world"
      , FontRanges.addChar 'Ꙑ'
      , FontRanges.addRanges FontRanges.Korean
      ]
@

-}

module DearImGui.FontAtlas
  ( -- * Main types
    Raw.Font(..)
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
  , glyphExtraSpacing
  , glyphOffset
  , glyphRanges
  , glyphMinAdvanceX
  , glyphMaxAdvanceX
  , mergeMode
  , fontBuilderFlags
  , rasterizerMultiply
  , ellipsisChar

    -- ** Configuring ranges
  , Ranges(..)
  , RangesBuilderSetup(..)
  , addChar
  , addText
  , addRanges
  , addRangesRaw

  , pattern Latin
  , pattern Korean
  , pattern Japanese
  , pattern ChineseFull
  , pattern ChineseSimplifiedCommon
  , pattern Cyrillic
  , pattern Thai
  , pattern Vietnamese

    -- * Lower level types and functions
  -- , Raw.FontConfig(..)
  -- , Raw.GlyphRanges(..)
  , build
  , clear
  , setupFont
  , setupRanges
  , withRanges
  , withConfig
  , addFontFromFileTTF
  , addFontFromFileTTF_
  )
  where

-- base
import Data.Bool (bool)
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
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)

-- dear-imgui
import DearImGui.Raw.Font (Font(..))
import qualified DearImGui.Raw.Font as Raw
import DearImGui.Raw.Font.Config (FontConfig(..))
import qualified DearImGui.Raw.Font.Config as FontConfig
import DearImGui.Raw.Font.GlyphRanges (GlyphRanges(..), GlyphRangesBuilder(..))
import qualified DearImGui.Raw.Font.GlyphRanges as GlyphRanges

import DearImGui.Structs (ImVec2(..), ImWchar)

-- | Font setup data
data FontSource
  = DefaultFont
  | FromTTF FilePath Float (Maybe ConfigSetup) Ranges
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

-- | Glyph ranges settings, from presets to builder configuration.
data Ranges
  = RangesRaw GlyphRanges
  | RangesBuiltin GlyphRanges.Builtin
  | RangesBuilder RangesBuilderSetup

-- | Basic Latin, Extended Latin
pattern Latin :: Ranges
pattern Latin = RangesBuiltin GlyphRanges.Latin

-- | Default + Korean characters
pattern Korean :: Ranges
pattern Korean = RangesBuiltin GlyphRanges.Korean

-- | Default + Hiragana, Katakana, Half-Width, Selection of 2999 Ideographs
pattern Japanese :: Ranges
pattern Japanese = RangesBuiltin GlyphRanges.Japanese

-- | Default + Half-Width + Japanese Hiragana/Katakana + full set of about 21000 CJK Unified Ideographs
pattern ChineseFull :: Ranges
pattern ChineseFull = RangesBuiltin GlyphRanges.ChineseFull

-- | Default + Half-Width + Japanese Hiragana/Katakana + set of 2500 CJK Unified Ideographs for common simplified Chinese
pattern ChineseSimplifiedCommon :: Ranges
pattern ChineseSimplifiedCommon = RangesBuiltin GlyphRanges.ChineseSimplifiedCommon

-- | Default + about 400 Cyrillic characters
pattern Cyrillic :: Ranges
pattern Cyrillic = RangesBuiltin GlyphRanges.Cyrillic

-- | Default + Thai characters
pattern Thai :: Ranges
pattern Thai = RangesBuiltin GlyphRanges.Thai

-- | Default + Vietnamese characters
pattern Vietnamese :: Ranges
pattern Vietnamese = RangesBuiltin GlyphRanges.Vietnamese


-- | Ranges builder monoid interface to be executed through 'buildRanges'.
--
-- @
-- addRanges FontRanges.DefaultRanges <> addText "Привет"
-- @
newtype RangesBuilderSetup = RangesBuilderSetup
  { applyToBuilder :: GlyphRangesBuilder -> IO ()
  }

instance Semigroup RangesBuilderSetup where
  RangesBuilderSetup f <> RangesBuilderSetup g =
    RangesBuilderSetup \fc -> f fc >> g fc

instance Monoid RangesBuilderSetup where
  mempty = RangesBuilderSetup (const mempty)

-- | Rebuild font atlas with provided configuration
-- and return corresponding structure of font handles
-- to be used with 'withFont'.
--
-- Accepts any 'Traversable' instance, so you are free to use
-- lists, maps or custom structures.
rebuild :: (MonadIO m, Traversable t) => t FontSource -> m (t Font)
rebuild sources = liftIO $ Managed.with action pure
  where
    action = do
      clear
      fonts <- traverse setupFont sources
      build
      return fonts

-- | Reset font atlas, clearing internal data
--
-- Alias for 'Raw.clearFontAtlas'
clear :: (MonadIO m) => m ()
clear = Raw.clearFontAtlas

-- | Build font atlas
--
-- Alias for 'Raw.buildFontAtlas'
build :: (MonadIO m) => m ()
build = Raw.buildFontAtlas

-- | Load a font from TTF file.
--
-- Specify font path and atlas glyph size.
--
-- Use 'Raw.addFontDefault' if you want to retain built-in font too.
--
-- Call 'build' after adding all the fonts,
-- particularly if you're loading them from memory or use custom glyphs.
-- Or stick to `rebuild` function.
--
-- Call backend-specific `CreateFontsTexture` before using 'newFrame'.
addFontFromFileTTF :: MonadIO m
  => FilePath               -- ^ Font file path
  -> Float                  -- ^ Font size in pixels
  -> Maybe FontConfig   -- ^ Configuration data
  -> Maybe GlyphRanges  -- ^ Glyph ranges to use
  -> m (Maybe Font)     -- ^ Returns font handle, if added successfully
addFontFromFileTTF font size config ranges = liftIO do
  res@(Font ptr) <- withCString font \fontPtr ->
    Raw.addFontFromFileTTF
      fontPtr
      (CFloat size)
      (fromMaybe (FontConfig nullPtr) config)
      (fromMaybe (GlyphRanges nullPtr) ranges)
  pure $
    if castPtr ptr == nullPtr
      then Nothing
      else Just res
      -- FIXME: turn off asserts, so it would work

addFontFromFileTTF_ :: MonadIO m
  => FilePath           -- ^ Font file path
  -> Float              -- ^ Font size in pixels
  -> m (Maybe Raw.Font) -- ^ Returns font handle, if added successfully
addFontFromFileTTF_ font size =
  addFontFromFileTTF font size Nothing Nothing

-- | Load a font with provided configuration, return its handle
-- and defer range builder and config destructors, if needed.
setupFont :: (MonadManaged m) => FontSource -> m Font
setupFont = \case
  DefaultFont ->
    Raw.addFontDefault
  FromTTF path size configSetup ranges -> do
    glyphRanges' <- setupRanges ranges
    config <- managed (withConfig configSetup)
    mFont <- addFontFromFileTTF path size config glyphRanges'
    case mFont of
      Nothing ->
        liftIO . fail $ "Couldn't load font from " <> path
      Just font ->
        pure font

-- | Configure glyph ranges with provided configuration, return a handle
-- and defer builder destructors, if needed.
setupRanges :: (MonadManaged m) => Ranges -> m (Maybe GlyphRanges)
setupRanges = \case
  RangesRaw ranges ->
    pure $ Just ranges
  RangesBuiltin builtin ->
    pure $ GlyphRanges.builtinSetup builtin
  RangesBuilder settings -> do
    built <- managed $ withRanges settings
    pure $ Just built

-- | Perform glyph ranges build based on provided configuration,
-- and execute a computation with built glyph ranges.
withRanges :: (MonadUnliftIO m) => RangesBuilderSetup -> (GlyphRanges -> m a) -> m a
withRanges (RangesBuilderSetup setup) fn =
  bracket acquire release execute
  where
    acquire = do
      builder <- GlyphRanges.new
      liftIO $ setup builder
      rangesVec <- GlyphRanges.buildRangesVector builder
      return (rangesVec, builder)

    release (rangesVec, builder) = do
      GlyphRanges.destroyRangesVector rangesVec
      GlyphRanges.destroy builder

    execute (rangesVec, _) =
      fn (GlyphRanges.fromRangesVector rangesVec)

-- | Configure font config with provided setup,
-- and execute a computation with built object.
-- return its handle and list of resource destructors.
withConfig :: (MonadUnliftIO m) => Maybe ConfigSetup -> (Maybe FontConfig -> m a) -> m a
withConfig mSetup action =
  case mSetup of
    Nothing ->
      action Nothing
    Just (ConfigSetup setup) ->
      bracket acquire (FontConfig.destroy) (action . Just)
      where
        acquire = do
          config <- FontConfig.new
          liftIO $ setup config
          return config

-- | Single Unicode character
addChar :: ImWchar -> RangesBuilderSetup
addChar char =
  RangesBuilderSetup \builder ->
    GlyphRanges.addChar builder char

-- | UTF-8 string
addText :: String -> RangesBuilderSetup
addText str =
  RangesBuilderSetup \builder ->
    withCString str (GlyphRanges.addText builder)

-- | Existing ranges (as is)
addRangesRaw :: GlyphRanges -> RangesBuilderSetup
addRangesRaw ranges =
  RangesBuilderSetup \builder ->
    GlyphRanges.addRanges builder ranges

-- | Existing ranges (through settings interface)
addRanges :: Ranges -> RangesBuilderSetup
addRanges = \case
  RangesRaw ranges ->
    addRangesRaw ranges
  RangesBuilder settings ->
    settings
  RangesBuiltin builtin ->
    addRangesRaw (GlyphRanges.getBuiltin builtin)

-- | TTF/OTF data ownership taken by the container ImFontAtlas (will delete memory itself).
--
-- By default, it is @true@
fontDataOwnedByAtlas :: Bool -> ConfigSetup
fontDataOwnedByAtlas value =
  ConfigSetup \fc ->
    FontConfig.setFontDataOwnedByAtlas fc (bool 0 1 value)

-- | Index of font within TTF/OTF file.
--
-- By default, it is @0@
fontNo :: Int -> ConfigSetup
fontNo value =
  ConfigSetup \fc ->
    FontConfig.setFontNo fc (fromIntegral value)

-- | Size in pixels for rasterizer
--
-- More or less maps to the resulting font height.
--
-- Implicitly set by @addFont...@ functions.
sizePixels :: Float -> ConfigSetup
sizePixels value =
  ConfigSetup \fc ->
    FontConfig.setSizePixels fc (CFloat value)

-- | Rasterize at higher quality for sub-pixel positioning.
--
-- Note: the difference between 2 and 3 is minimal so you can reduce this to 2 to save memory.
-- Read https://github.com/nothings/stb/blob/master/tests/oversample/README.md for details.
--
-- By default, it is @3@
oversampleH :: Int -> ConfigSetup
oversampleH value =
  ConfigSetup \fc ->
    FontConfig.setOversampleH fc (fromIntegral value)

-- | Rasterize at higher quality for sub-pixel positioning.
--
-- This is not really useful as we don't use sub-pixel positions on the Y axis.
--
-- By default, it is @1@
oversampleV :: Int -> ConfigSetup
oversampleV value =
  ConfigSetup \fc ->
    FontConfig.setOversampleV fc (fromIntegral value)

-- | Align every glyph to pixel boundary.
--
-- Useful if you are merging a non-pixel aligned font with the default font.
-- If enabled, you can set OversampleH/V to 1.
--
-- By default, it is @false@
pixelSnapH :: Bool -> ConfigSetup
pixelSnapH value =
  ConfigSetup \fc ->
    FontConfig.setPixelSnapH fc (bool 0 1 value)

-- | Extra spacing (in pixels) between glyphs.
--
-- Only X axis is supported for now.
--
-- By default, it is @0, 0@
glyphExtraSpacing :: (Float, Float) -> ConfigSetup
glyphExtraSpacing (x, y) =
  ConfigSetup \fc ->
    Foreign.with (ImVec2 x y) (FontConfig.setGlyphExtraSpacing fc)

-- | Offset all glyphs from this font input.
--
-- By default, it is @0, 0@
glyphOffset :: (Float, Float) -> ConfigSetup
glyphOffset (x, y) =
  ConfigSetup \fc ->
    Foreign.with (ImVec2 x y) (FontConfig.setGlyphOffset fc)

-- | Pointer to a user-provided list of Unicode range.
--
-- 2 values per range, inclusive. Zero-terminated list.
--
-- THE ARRAY DATA NEEDS TO PERSIST AS LONG AS THE FONT IS ALIVE.
--
-- By default, it is @NULL@
glyphRanges :: GlyphRanges -> ConfigSetup
glyphRanges value =
  ConfigSetup \fc ->
    FontConfig.setGlyphRanges fc value

-- | Minimum AdvanceX for glyphs.
--
-- Set Min to align font icons, set both Min/Max to enforce mono-space font.
--
-- By default, it is @0@
glyphMinAdvanceX :: Float -> ConfigSetup
glyphMinAdvanceX value =
  ConfigSetup \fc ->
    FontConfig.setGlyphMinAdvanceX fc (CFloat value)

-- | Maximum AdvanceX for glyphs.
--
-- By default, it is @FLT_MAX@.
glyphMaxAdvanceX :: Float -> ConfigSetup
glyphMaxAdvanceX value =
  ConfigSetup \fc ->
    FontConfig.setGlyphMaxAdvanceX fc (CFloat value)

-- | Merge into previous ImFont, so you can combine multiple inputs font into one ImFont.
--
-- e.g. ASCII font + icons + Japanese glyphs.
-- You may want to use @GlyphOffset.y@ when merging font of different heights.
--
-- By default, it is @false@
mergeMode :: Bool -> ConfigSetup
mergeMode value =
  ConfigSetup \fc ->
    FontConfig.setMergeMode fc (bool 0 1 value)

-- | Settings for custom font GlyphRanges.
--
-- THIS IS BUILDER IMPLEMENTATION DEPENDENT.
--
-- By default, it is @0@. Leave it so if unsure.
fontBuilderFlags :: Int -> ConfigSetup
fontBuilderFlags value =
  ConfigSetup \fc ->
    FontConfig.setFontBuilderFlags fc (fromIntegral value)

-- | Brighten (>1.0f) or darken (<1.0f) font output.
--
-- Brightening small fonts may be a good workaround to make them more readable.
--
-- By default, it is @1.0f@.
rasterizerMultiply :: Float -> ConfigSetup
rasterizerMultiply value =
  ConfigSetup \fc ->
    FontConfig.setRasterizerMultiply fc (CFloat value)

-- | Explicitly specify unicode codepoint of ellipsis character.
--
-- When fonts are being merged first specified ellipsis will be used.
--
-- By default, it is @-1@
ellipsisChar :: ImWchar -> ConfigSetup
ellipsisChar value =
  ConfigSetup \fc ->
    FontConfig.setEllipsisChar fc value
