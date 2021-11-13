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
Module: DearImGui.Fonts

ImGui module, exporting fonts API.
-}

module DearImGui.Fonts 
  ( -- * Types
    Raw.Font(..)
  , Raw.FontConfig(..)
  , Raw.GlyphRanges(..)
    -- * Adding fonts
  , Raw.addFontDefault
  , addFontFromFileTTF
  , addFontFromFileTTF'
  , Raw.addFontFromMemoryTTF
    -- * Using fonts
  , withFont
  , Raw.pushFont
  , Raw.popFont
    -- * Glyph ranges presets
  , Raw.getGlyphRangesDefault
  , Raw.getGlyphRangesKorean
  , Raw.getGlyphRangesJapanese
  , Raw.getGlyphRangesChineseFull
  , Raw.getGlyphRangesChineseSimplifiedCommon
  , Raw.getGlyphRangesCyrillic
  , Raw.getGlyphRangesThai
  , Raw.getGlyphRangesVietnamese
    -- * Atlas management
  , Raw.clearFontAtlas
  , Raw.buildFontAtlas
  )
  where

-- base
import Data.Maybe
  ( fromMaybe )
import Foreign
import Foreign.C

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- unlift
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket_)

-- dear-imgui
import qualified DearImGui.Raw.Fonts as Raw


-- | Load a font from TTF file.
--
-- Specify font path and atlas glyph size.
--
-- Use 'addFontDefault' if you want to retain built-in font too.
--
-- Call 'buildFontAtlas' after adding all the fonts, 
-- particularly if you're loading them from memory or use custom glyphs.
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
addFontFromFileTTF' font size mFontConfig mGlyphRanges = liftIO do
  let 
    fontConfig = fromMaybe (Raw.FontConfig nullPtr) mFontConfig
    glyphRanges = fromMaybe (Raw.GlyphRanges nullPtr) mGlyphRanges
  res@(Raw.Font ptr) <- withCString font \fontPtr ->
    Raw.addFontFromFileTTF fontPtr (CFloat size) fontConfig glyphRanges
  pure $
    if castPtr ptr == nullPtr
      then Nothing
      else Just res


-- | Render widgets inside the block using provided font.
withFont :: MonadUnliftIO m => Raw.Font -> m a -> m a
withFont font = bracket_ (Raw.pushFont font) Raw.popFont