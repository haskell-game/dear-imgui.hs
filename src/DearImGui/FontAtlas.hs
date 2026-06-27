{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.FontAtlas

Font atlas builder, accompanied with lower-level functions.

Since imgui 1.92, specifying glyph ranges is no longer necessary with modern
backends — glyphs are loaded on demand.

@
import qualified DearImGui.FontAtlas as FontAtlas

prepareAtlas =
  FontAtlas.rebuild
    [ FontAtlas.FromTTF "comic-sans-mono.ttf" 13 csOptions
    , FontAtlas.Default
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
  -- , Raw.FontConfig(..)
  , build
  , clear
  , setupFont
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
import DearImGui.Raw.Font.GlyphRanges (GlyphRanges(..))

import DearImGui.Structs (ImVec2(..), ImWchar)

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
-- Specify font path and optionally a size. Pass 'Nothing' to let imgui
-- pick the size automatically (recommended since 1.92).
--
-- Use 'Raw.addFontDefault' if you want to retain built-in font too.
--
-- Call 'build' after adding all the fonts,
-- particularly if you're loading them from memory or use custom glyphs.
-- Or stick to `rebuild` function.
addFontFromFileTTF :: MonadIO m
  => FilePath               -- ^ Font file path
  -> Maybe Float            -- ^ Font size in pixels (Nothing = automatic)
  -> Maybe FontConfig       -- ^ Configuration data
  -> m (Maybe Font)     -- ^ Returns font handle, if added successfully
addFontFromFileTTF font size config = liftIO do
  res@(Font ptr) <- withCString font \fontPtr ->
    Raw.addFontFromFileTTF
      fontPtr
      (CFloat $ fromMaybe 0 size)
      (fromMaybe (FontConfig nullPtr) config)
  pure $
    if castPtr ptr == nullPtr
      then Nothing
      else Just res
      -- FIXME: turn off asserts, so it would work

addFontFromFileTTF_ :: MonadIO m
  => FilePath           -- ^ Font file path
  -> m (Maybe Raw.Font) -- ^ Returns font handle, if added successfully
addFontFromFileTTF_ font =
  addFontFromFileTTF font Nothing Nothing

-- | Load a font with provided configuration, return its handle
-- and defer config destructors, if needed.
setupFont :: (MonadManaged m) => FontSource -> m Font
setupFont = \case
  DefaultFont ->
    Raw.addFontDefault
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

-- | Offset all glyphs from this font input.
--
-- By default, it is @0, 0@
glyphOffset :: (Float, Float) -> ConfigSetup
glyphOffset (x, y) =
  ConfigSetup \fc ->
    Foreign.with (ImVec2 x y) (FontConfig.setGlyphOffset fc)

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
    FontConfig.setGlyphExcludeRanges fc (GlyphRanges ptr)

-- | Extra spacing (in pixels) between glyphs.
--
-- By default, it is @0@
glyphExtraAdvanceX :: Float -> ConfigSetup
glyphExtraAdvanceX x =
  ConfigSetup \fc ->
    FontConfig.setGlyphExtraAdvanceX fc (CFloat x)

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

-- | Settings for custom font loader.
--
-- THIS IS LOADER IMPLEMENTATION DEPENDENT.
--
-- By default, it is @0@. Leave it so if unsure.
fontLoaderFlags :: Int -> ConfigSetup
fontLoaderFlags value =
  ConfigSetup \fc ->
    FontConfig.setFontLoaderFlags fc (fromIntegral value)

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
