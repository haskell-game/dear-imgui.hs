{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}


{-| Fonts

It includes default atlas management, font configuration and glyph ranges.

-}

module DearImGui.Raw.Fonts
  ( -- * Types
    Font(..)
  , FontConfig(..)
  , GlyphRanges(..)
    -- * Adding fonts
  , addFontDefault
  , addFontFromFileTTF
  , addFontFromMemoryTTF
    -- * Using fonts
  , pushFont
  , popFont
    -- * Glyph ranges presets
  , getGlyphRangesDefault
  , getGlyphRangesKorean
  , getGlyphRangesJapanese
  , getGlyphRangesChineseFull
  , getGlyphRangesChineseSimplifiedCommon
  , getGlyphRangesCyrillic
  , getGlyphRangesThai
  , getGlyphRangesVietnamese
    -- * Atlas management
  , clearFontAtlas
  , buildFontAtlas
  )
  where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign ( Ptr, castPtr )
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

-- dear-imgui
import DearImGui.Context
  ( imguiContext )
import DearImGui.Structs

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"


-- | Font runtime data handle
--
-- Wraps @ImFont*@.
newtype Font = Font (Ptr ImFont)

-- | Font configuration data handle
--
-- Wraps @ImFontConfig*@.
newtype FontConfig = FontConfig (Ptr ImFontConfig)

-- | Glyph ranges handle
--
-- Wraps @ImWchar*@.
newtype GlyphRanges = GlyphRanges (Ptr ImWchar)


-- | Add the default font (@ProggyClean.ttf@, 13 px) to the atlas.
addFontDefault :: MonadIO m
  => m Font   -- ^ Returns font handle for future usage
addFontDefault = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontDefault();
    }
  |]

-- | Add a custom OTF/TTF font from a file.
addFontFromFileTTF :: MonadIO m
  => CString     -- ^ Font file path
  -> CFloat      -- ^ Font size in pixels
  -> FontConfig  -- ^ Configuration data
  -> GlyphRanges -- ^ Glyph ranges to use
  -> m Font      -- ^ Returns font handle for future usage
addFontFromFileTTF filenamePtr sizePixels (FontConfig fontConfig) (GlyphRanges glyphRanges) = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontFromFileTTF(
        $(char* filenamePtr),
        $(float sizePixels),
        $(ImFontConfig* fontConfig),
        $(ImWchar* glyphRanges));
    }
  |]

-- | Transfer a buffer with TTF data to font atlas builder.
addFontFromMemoryTTF :: MonadIO m => CStringLen -> CFloat -> FontConfig -> GlyphRanges -> m Font
addFontFromMemoryTTF (castPtr -> fontDataPtr, fromIntegral -> fontSize) sizePixels (FontConfig fontConfig) (GlyphRanges glyphRanges) = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontFromMemoryTTF(
        $(void* fontDataPtr),
        $(int fontSize),
        $(float sizePixels),
        $(ImFontConfig* fontConfig),
        $(ImWchar* glyphRanges)
      );
    }
  |]


-- | Pushes a font into the parameters stack,
-- so ImGui would render following text using it.
pushFont :: MonadIO m => Font -> m ()
pushFont (Font font) = liftIO do
  [C.exp| void { PushFont($(ImFont* font)); } |]

-- | Pops a font pushed into the parameters stack
-- 
-- Should be called only after a corresponding 'pushFont' call.
popFont :: MonadIO m => m ()
popFont = liftIO do
  [C.exp| void { PopFont(); } |]
  

-- | Basic Latin, Extended Latin
getGlyphRangesDefault :: GlyphRanges
getGlyphRangesDefault = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesDefault();
    }
  |]

-- | Default + Korean characters
getGlyphRangesKorean :: GlyphRanges
getGlyphRangesKorean = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesKorean();
    }
  |]

-- | Default + Hiragana, Katakana, Half-Width, Selection of 2999 Ideographs
getGlyphRangesJapanese :: GlyphRanges
getGlyphRangesJapanese = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesJapanese();
    }
  |]

-- | Default + Half-Width + Japanese Hiragana/Katakana + full set of about 21000 CJK Unified Ideographs
getGlyphRangesChineseFull :: GlyphRanges
getGlyphRangesChineseFull = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesChineseFull();
    }
  |]

-- | Default + Half-Width + Japanese Hiragana/Katakana + set of 2500 CJK Unified Ideographs for common simplified Chinese
getGlyphRangesChineseSimplifiedCommon :: GlyphRanges
getGlyphRangesChineseSimplifiedCommon = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesChineseSimplifiedCommon();
    }
  |]

-- | Default + about 400 Cyrillic characters
getGlyphRangesCyrillic :: GlyphRanges
getGlyphRangesCyrillic = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesCyrillic();
    }
  |]

-- | Default + Thai characters
getGlyphRangesThai :: GlyphRanges
getGlyphRangesThai = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesThai();
    }
  |]

-- | Default + Vietnamese characters
getGlyphRangesVietnamese :: GlyphRanges
getGlyphRangesVietnamese = unsafePerformIO do
  GlyphRanges <$> [C.block|
    const ImWchar* {
      return GetIO().Fonts->GetGlyphRangesVietnamese();
    }
  |]

-- | Explicitly build pixels data for the atlas.
buildFontAtlas :: MonadIO m => m ()
buildFontAtlas = liftIO do
  [C.block|
    void {
      GetIO().Fonts->Build();
    }
  |]

-- | Clear all font atlas input and output data
clearFontAtlas :: MonadIO m => m ()
clearFontAtlas = liftIO do
  [C.block|
    void {
      GetIO().Fonts->Clear();
    }
  |]
