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

{- TODO

struct ImFontConfig
{
    void*           FontData;               //          // TTF/OTF data
    int             FontDataSize;           //          // TTF/OTF data size
    bool            FontDataOwnedByAtlas;   // true     // TTF/OTF data ownership taken by the container ImFontAtlas (will delete memory itself).
    int             FontNo;                 // 0        // Index of font within TTF/OTF file
    float           SizePixels;             //          // Size in pixels for rasterizer (more or less maps to the resulting font height).
    int             OversampleH;            // 3        // Rasterize at higher quality for sub-pixel positioning. Note the difference between 2 and 3 is minimal so you can reduce this to 2 to save memory. Read https://github.com/nothings/stb/blob/master/tests/oversample/README.md for details.
    int             OversampleV;            // 1        // Rasterize at higher quality for sub-pixel positioning. This is not really useful as we don't use sub-pixel positions on the Y axis.
    bool            PixelSnapH;             // false    // Align every glyph to pixel boundary. Useful e.g. if you are merging a non-pixel aligned font with the default font. If enabled, you can set OversampleH/V to 1.
    ImVec2          GlyphExtraSpacing;      // 0, 0     // Extra spacing (in pixels) between glyphs. Only X axis is supported for now.
    ImVec2          GlyphOffset;            // 0, 0     // Offset all glyphs from this font input.
    const ImWchar*  GlyphRanges;            // NULL     // Pointer to a user-provided list of Unicode range (2 value per range, values are inclusive, zero-terminated list). THE ARRAY DATA NEEDS TO PERSIST AS LONG AS THE FONT IS ALIVE.
    float           GlyphMinAdvanceX;       // 0        // Minimum AdvanceX for glyphs, set Min to align font icons, set both Min/Max to enforce mono-space font
    float           GlyphMaxAdvanceX;       // FLT_MAX  // Maximum AdvanceX for glyphs
    bool            MergeMode;              // false    // Merge into previous ImFont, so you can combine multiple inputs font into one ImFont (e.g. ASCII font + icons + Japanese glyphs). You may want to use GlyphOffset.y when merge font of different heights.
    unsigned int    FontBuilderFlags;       // 0        // Settings for custom font builder. THIS IS BUILDER IMPLEMENTATION DEPENDENT. Leave as zero if unsure.
    float           RasterizerMultiply;     // 1.0f     // Brighten (>1.0f) or darken (<1.0f) font output. Brightening small fonts may be a good workaround to make them more readable.
    ImWchar         EllipsisChar;           // -1       // Explicitly specify unicode codepoint of ellipsis character. When fonts are being merged first specified ellipsis will be used.

    // [Internal]
    char            Name[40];               // Name (strictly to ease debugging)
    ImFont*         DstFont;

    IMGUI_API ImFontConfig();
};

struct ImFontGlyphRangesBuilder
{
    ImVector<ImU32> UsedChars;            // Store 1-bit per Unicode code point (0=unused, 1=used)

    ImFontGlyphRangesBuilder()              { Clear(); }
    inline void     Clear()                 { int size_in_bytes = (IM_UNICODE_CODEPOINT_MAX + 1) / 8; UsedChars.resize(size_in_bytes / (int)sizeof(ImU32)); memset(UsedChars.Data, 0, (size_t)size_in_bytes); }
    inline bool     GetBit(size_t n) const  { int off = (int)(n >> 5); ImU32 mask = 1u << (n & 31); return (UsedChars[off] & mask) != 0; }  // Get bit n in the array
    inline void     SetBit(size_t n)        { int off = (int)(n >> 5); ImU32 mask = 1u << (n & 31); UsedChars[off] |= mask; }               // Set bit n in the array
    inline void     AddChar(ImWchar c)      { SetBit(c); }                      // Add character
    IMGUI_API void  AddText(const char* text, const char* text_end = NULL);     // Add string (each character of the UTF-8 string are added)
    IMGUI_API void  AddRanges(const ImWchar* ranges);                           // Add ranges, e.g. builder.AddRanges(ImFontAtlas::GetGlyphRangesDefault()) to force add all of ASCII/Latin+Ext
    IMGUI_API void  BuildRanges(ImVector<ImWchar>* out_ranges);                 // Output new ranges
};

-}