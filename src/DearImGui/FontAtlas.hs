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
Supposed for qualified import, like such:

@
-- import qualified DearImGui.FontAtlas as FontAtlas
FontAtlas.rebuild
  [ FontAtlas.FileTTF "comic-sans-mono.ttf" 14
      ( FontAtlas.fontNo 1 <> FontAtlas.glyphOffset (0, -1))
      ( RangeBuilder $ FontAtlas.addText "Hello world"
                    <> FontRanges.addChar 'Ꙑ'
                    <> FontRanges.addRanges FontRanges.Korean
      )
  , FontAtlas.Default
  ]
@

-}

module DearImGui.FontAtlas
  ( -- * Main types
    Raw.Font(..)
  , FontSetting(..)
    -- * Building atlas
  , rebuild
    -- * Configuring fonts
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
    -- * Configuring ranges
  , RangesSetting(..)
  , RangesBuilderSetup(..)
  , addChar
  , addText
  , addRanges
  , addRangesRaw
    -- * Using fonts
  , withFont
  , Raw.pushFont
  , Raw.popFont
    -- * Lower level types and functions
  , Raw.FontConfig(..)
  , Raw.GlyphRanges(..)
  , build
  , clear
  , setupFont
  , setupRanges
  , withRanges
  , withConfig
  , addFontFromFileTTF
  , addFontFromFileTTF'
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
import qualified Control.Monad.Managed as Managed
import Control.Monad.Managed (MonadManaged)

-- unlift
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, bracket_)

-- dear-imgui
import DearImGui.Structs (ImVec2(..), ImWchar)
import qualified DearImGui.Raw.Fonts as Raw
import qualified DearImGui.Raw.FontConfig as FontConfig
import qualified DearImGui.Raw.GlyphRangesBuilder as Builder
import DearImGui.Raw.Fonts (Font(..), FontConfig(..), GlyphRanges(..))


-- | Font setup data
data FontSetting
  = DefaultFont
  | FromTTF FilePath Float (Maybe ConfigSetup) RangesSetting
  -- TODO: FromMemory

-- | Font config monoid interface to be used in 'FontSetting'.
--
-- Usage example: @mergeMode True <> fontNo 1@
data ConfigSetup = ConfigSetup
  { applyToConfig :: FontConfig -> IO () }

instance Semigroup ConfigSetup where
  (ConfigSetup f) <> (ConfigSetup g) =
    ConfigSetup $ \fc -> f fc >> g fc
instance Monoid ConfigSetup where
  mempty = ConfigSetup $ \_ -> return ()

-- | Glyph ranges settings, from presets to builder configuration.
data RangesSetting
  = DefaultRanges
  | Ranges GlyphRanges
  | RangesBuilder RangesBuilderSetup
  -- sugar section
  | Korean
  | Japanese
  | ChineseFull
  | ChineseSimplifiedCommon
  | Cyrillic
  | Thai
  | Vietnamese

-- | Ranges builder monoid interface to be executed through 'buildRanges'.
--
-- Usage example: @addRanges FontRanges.DefaultRanges <> addText "Привет"@
data RangesBuilderSetup = RangesBuilderSetup
  { applyToBuilder :: Builder.GlyphRangesBuilder -> IO () }

instance Semigroup RangesBuilderSetup where
  (RangesBuilderSetup f) <> (RangesBuilderSetup g) =
    RangesBuilderSetup $ \fc -> f fc >> g fc
instance Monoid RangesBuilderSetup where
  mempty = RangesBuilderSetup $ \_ -> return ()


-- | Rebuild font atlas with provided configuration
-- and return corresponding structure of font handles
-- to be used with 'withFont'.
-- Accepts any 'Traversable' instance, so you are free to use
-- lists, maps or custom structures.
rebuild :: (MonadIO m, Traversable t) => t FontSetting -> m (t Font)
rebuild fonts = liftIO $ Managed.with m return
  where
    m = do
      Raw.clearFontAtlas
      handles <- traverse setupFont fonts
      Raw.buildFontAtlas
      return handles


-- | Render widgets inside the block using provided font.
withFont :: MonadUnliftIO m => Raw.Font -> m a -> m a
withFont font = bracket_ (Raw.pushFont font) Raw.popFont


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
  => FilePath -- ^ Font file path
  -> Float    -- ^ Font size in pixels
  -> m (Maybe Raw.Font)  -- ^ Returns font handle, if added successfully
addFontFromFileTTF font size =
  addFontFromFileTTF' font size Nothing Nothing

-- | Extended version of 'addFontFromFileTTF',
-- supporting font configuration and glyph ranges.
--
addFontFromFileTTF' :: MonadIO m
  => FilePath -- ^ Font file path
  -> Float    -- ^ Font size in pixels
  -> Maybe Raw.FontConfig   -- ^ Configuration data
  -> Maybe Raw.GlyphRanges  -- ^ Glyph ranges to use
  -> m (Maybe Raw.Font) -- ^ Returns font handle, if added successfully
addFontFromFileTTF' font size mConfig mRanges = liftIO do
  let
    config = fromMaybe (Raw.FontConfig nullPtr) mConfig
    ranges = fromMaybe (Raw.GlyphRanges nullPtr) mRanges
  res@(Raw.Font ptr) <- withCString font \fontPtr ->
    Raw.addFontFromFileTTF fontPtr (CFloat size) config ranges
  pure $
    if castPtr ptr == nullPtr
      then Nothing
      else Just res
      -- FIXME: turn off asserts, so it would work


-- | Load a font with provided configuration, return its handle
-- and defer range builder and config destructors, if needed.
setupFont :: (MonadManaged m) => FontSetting -> m Font
setupFont DefaultFont = Raw.addFontDefault
setupFont (FromTTF path size fcs rs) = do
  ranges <- setupRanges rs
  config <- Managed.managed (withConfig fcs)
  mFont <- addFontFromFileTTF' path size config ranges
  case mFont of
    Nothing -> liftIO . fail $ "Couldn't load font from " <> path
    Just font -> return font

-- | Configure glyph ranges with provided configuration, return a handle
-- and defer builder destructors, if needed.
setupRanges :: (MonadManaged m) => RangesSetting -> m (Maybe GlyphRanges)
setupRanges (Ranges ranges) = return $ Just ranges
setupRanges DefaultRanges = return Nothing
setupRanges Korean = return $ Just Raw.getGlyphRangesKorean
setupRanges Japanese = return $ Just Raw.getGlyphRangesJapanese
setupRanges ChineseFull = return $ Just Raw.getGlyphRangesChineseFull
setupRanges ChineseSimplifiedCommon = return $ Just Raw.getGlyphRangesChineseSimplifiedCommon
setupRanges Cyrillic = return $ Just Raw.getGlyphRangesCyrillic
setupRanges Thai = return $ Just Raw.getGlyphRangesThai
setupRanges Vietnamese = return $ Just Raw.getGlyphRangesVietnamese
setupRanges (RangesBuilder settings) = do
  ranges <- Managed.managed $ withRanges settings
  return (Just ranges)


-- | Perform glyph ranges build based on provided configuration,
-- and execute a computation with built glyph ranges.
withRanges :: (MonadUnliftIO m) => RangesBuilderSetup
           -> (GlyphRanges -> m a) -> m a
withRanges (RangesBuilderSetup setup) fn =
  bracket acquire release execute
  where
    acquire = do
      builder <- Builder.new
      liftIO $ setup builder
      rangesVec <- Builder.buildRanges builder
      return (rangesVec, builder)
    release (rangesVec, builder) = do
      Builder.destroyRangesVector rangesVec
      Builder.destroy builder
    execute (rangesVec, _) =
      fn (Builder.fromRangesVector rangesVec)

-- | Configure font config with provided setup,
-- and execute a computation with built object.
-- return its handle and list of resource destructors.
withConfig :: (MonadUnliftIO m) => Maybe ConfigSetup
           -> (Maybe FontConfig -> m a) -> m a
withConfig Nothing fn = fn Nothing
withConfig (Just (ConfigSetup setup)) fn =
  bracket acquire (FontConfig.destroy) (fn . Just)
  where
    acquire = do
      config <- FontConfig.new
      liftIO $ setup config
      return config


-- | Single Unicode character
addChar :: ImWchar -> RangesBuilderSetup
addChar char = RangesBuilderSetup
  (\builder -> Builder.addChar builder char)

-- | UTF-8 string
addText :: String -> RangesBuilderSetup
addText str = RangesBuilderSetup
  (\builder -> withCString str (Builder.addText builder))

-- | Existing ranges (as is)
addRangesRaw :: GlyphRanges -> RangesBuilderSetup
addRangesRaw ranges = RangesBuilderSetup
  (\builder -> Builder.addRanges builder ranges)

-- | Existing ranges (through settings interface)
addRanges :: RangesSetting -> RangesBuilderSetup
addRanges (Ranges ranges) = addRangesRaw ranges
addRanges DefaultRanges = addRangesRaw Raw.getGlyphRangesDefault
addRanges Korean = addRangesRaw Raw.getGlyphRangesKorean
addRanges Japanese = addRangesRaw Raw.getGlyphRangesJapanese
addRanges ChineseFull = addRangesRaw Raw.getGlyphRangesChineseFull
addRanges ChineseSimplifiedCommon = addRangesRaw Raw.getGlyphRangesChineseSimplifiedCommon
addRanges Cyrillic = addRangesRaw Raw.getGlyphRangesCyrillic
addRanges Thai = addRangesRaw Raw.getGlyphRangesThai
addRanges Vietnamese = addRangesRaw Raw.getGlyphRangesVietnamese
addRanges (RangesBuilder settings) = settings


-- | TTF/OTF data ownership taken by the container ImFontAtlas (will delete memory itself).
--
-- By default, it is @true@
fontDataOwnedByAtlas :: Bool -> ConfigSetup
fontDataOwnedByAtlas value = ConfigSetup
  (\fc -> FontConfig.setFontDataOwnedByAtlas fc (bool 0 1 value))

-- | Index of font within TTF/OTF file
--
-- By default, it is @0@
fontNo :: Int -> ConfigSetup
fontNo value = ConfigSetup
  (\fc -> FontConfig.setFontNo fc (fromIntegral value))

-- | Size in pixels for rasterizer (more or less maps to the resulting font height).
--
-- Implicitly set by @addFont...@ functions.
sizePixels :: Float -> ConfigSetup
sizePixels value = ConfigSetup
  (\fc -> FontConfig.setSizePixels fc (CFloat value))

-- | Rasterize at higher quality for sub-pixel positioning. Note the difference between 2 and 3 is minimal so you can reduce this to 2 to save memory. Read https://github.com/nothings/stb/blob/master/tests/oversample/README.md for details.
--
-- By default, it is @3@
oversampleH :: Int -> ConfigSetup
oversampleH value = ConfigSetup
  (\fc -> FontConfig.setOversampleH fc (fromIntegral value))

-- | Rasterize at higher quality for sub-pixel positioning. This is not really useful as we don't use sub-pixel positions on the Y axis.
--
-- By default, it is @1@
oversampleV :: Int -> ConfigSetup
oversampleV value = ConfigSetup
  (\fc -> FontConfig.setOversampleV fc (fromIntegral value))

-- | Align every glyph to pixel boundary. Useful e.g. if you are merging a non-pixel aligned font with the default font. If enabled, you can set OversampleH/V to 1.
--
-- By default, it is @false@
pixelSnapH :: Bool -> ConfigSetup
pixelSnapH value = ConfigSetup
  (\fc -> FontConfig.setPixelSnapH fc (bool 0 1 value))

-- | Extra spacing (in pixels) between glyphs. Only X axis is supported for now.
--
-- By default, it is @0, 0@
glyphExtraSpacing :: (Float, Float) -> ConfigSetup
glyphExtraSpacing (x, y) = ConfigSetup
  (\fc -> Foreign.with (ImVec2 x y) (FontConfig.setGlyphExtraSpacing fc))

-- | Offset all glyphs from this font input.
--
-- By default, it is @0, 0@
glyphOffset :: (Float, Float) -> ConfigSetup
glyphOffset (x, y) = ConfigSetup
  (\fc -> Foreign.with (ImVec2 x y) (FontConfig.setGlyphOffset fc))

-- | Pointer to a user-provided list of Unicode range (2 value per range, values are inclusive, zero-terminated list).
-- THE ARRAY DATA NEEDS TO PERSIST AS LONG AS THE FONT IS ALIVE.
--
-- By default, it is @NULL@
glyphRanges :: GlyphRanges -> ConfigSetup
glyphRanges value = ConfigSetup
  (\fc -> FontConfig.setGlyphRanges fc value)

-- | Minimum AdvanceX for glyphs, set Min to align font icons, set both Min/Max to enforce mono-space font
--
-- By default, it is @0@
glyphMinAdvanceX :: Float -> ConfigSetup
glyphMinAdvanceX value = ConfigSetup
  (\fc -> FontConfig.setGlyphMinAdvanceX fc (CFloat value))

-- | Maximum AdvanceX for glyphs
--
-- By default, it is @FLT_MAX@
glyphMaxAdvanceX :: Float -> ConfigSetup
glyphMaxAdvanceX value = ConfigSetup
  (\fc -> FontConfig.setGlyphMaxAdvanceX fc (CFloat value))

-- | Merge into previous ImFont, so you can combine multiple inputs font into one ImFont (e.g. ASCII font + icons + Japanese glyphs). You may want to use GlyphOffset.y when merge font of different heights.
--
-- By default, it is @false@
mergeMode :: Bool -> ConfigSetup
mergeMode value = ConfigSetup
  (\fc -> FontConfig.setMergeMode fc (bool 0 1 value))

-- | Settings for custom font builder.
-- THIS IS BUILDER IMPLEMENTATION DEPENDENT.
--
-- By default, it is @0@. Leave it so if unsure.
fontBuilderFlags :: Int -> ConfigSetup
fontBuilderFlags value = ConfigSetup
  (\fc -> FontConfig.setFontBuilderFlags fc (fromIntegral value))

-- | Brighten (>1.0f) or darken (<1.0f) font output.
-- Brightening small fonts may be a good workaround to make them more readable.
--
-- By default, it is @1.0f@
rasterizerMultiply :: Float -> ConfigSetup
rasterizerMultiply value = ConfigSetup
  (\fc -> FontConfig.setRasterizerMultiply fc (CFloat value))

-- | Explicitly specify unicode codepoint of ellipsis character. When fonts are being merged first specified ellipsis will be used.
--
-- By default, it is @-1@
ellipsisChar :: ImWchar -> ConfigSetup
ellipsisChar value = ConfigSetup
  (\fc -> FontConfig.setEllipsisChar fc value)
