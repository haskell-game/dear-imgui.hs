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


{-| Font configuration

IO functions to modify font config values.

-}

module DearImGui.Raw.Font.Config
  ( FontConfig(..)
  , new
  , destroy
    -- * Changing settings
  , setFontDataOwnedByAtlas
  , setFontNo
  , setSizePixels
  , setOversampleH
  , setOversampleV
  , setPixelSnapH
  , setGlyphExtraSpacing
  , setGlyphOffset
  , setGlyphRanges
  , setGlyphMinAdvanceX
  , setGlyphMaxAdvanceX
  , setMergeMode
  , setFontBuilderFlags
  , setRasterizerMultiply
  , setEllipsisChar
  )
  where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign ( Ptr )
import Foreign.C

-- dear-imgui
import DearImGui.Raw.Context
  ( imguiContext )
import DearImGui.Structs
import DearImGui.Raw.Font.GlyphRanges
  ( GlyphRanges(..) )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"

-- | Font configuration data handle
--
-- Wraps @ImFontConfig*@.
newtype FontConfig = FontConfig (Ptr ImFontConfig)

-- | Create an instance of config
new :: MonadIO m => m FontConfig
new = liftIO do
  FontConfig <$> [C.block|
    ImFontConfig* {
      return IM_NEW(ImFontConfig);
    }
  |]

-- | Destroy an instance of config
--
-- Should be used __after__ font atlas building.
destroy :: MonadIO m => FontConfig -> m ()
destroy (FontConfig config) = liftIO do
  [C.block|
    void {
      IM_DELETE($(ImFontConfig* config));
    }
  |]


-- | TTF/OTF data ownership taken by the container ImFontAtlas (will delete memory itself).
--
-- By default, it is @true@
setFontDataOwnedByAtlas :: MonadIO m => FontConfig -> CBool -> m ()
setFontDataOwnedByAtlas (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->FontDataOwnedByAtlas = $(bool value);
    }
  |]

-- | Index of font within TTF/OTF file
--
-- By default, it is @0@
setFontNo :: MonadIO m => FontConfig -> CInt -> m ()
setFontNo (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->FontNo = $(int value);
    }
  |]

-- | Size in pixels for rasterizer (more or less maps to the resulting font height).
--
-- Implicitly set by @addFont...@ functions.
setSizePixels :: MonadIO m => FontConfig -> CFloat -> m ()
setSizePixels (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->SizePixels = $(float value);
    }
  |]

-- | Rasterize at higher quality for sub-pixel positioning. Note the difference between 2 and 3 is minimal so you can reduce this to 2 to save memory. Read https://github.com/nothings/stb/blob/master/tests/oversample/README.md for details.
--
-- By default, it is @3@
setOversampleH :: MonadIO m => FontConfig -> CInt -> m ()
setOversampleH (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->OversampleH = $(int value);
    }
  |]

-- | Rasterize at higher quality for sub-pixel positioning. This is not really useful as we don't use sub-pixel positions on the Y axis.
--
-- By default, it is @1@
setOversampleV :: MonadIO m => FontConfig -> CInt -> m ()
setOversampleV (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->OversampleV = $(int value);
    }
  |]

-- | Align every glyph to pixel boundary. Useful e.g. if you are merging a non-pixel aligned font with the default font. If enabled, you can set OversampleH/V to 1.
--
-- By default, it is @false@
setPixelSnapH :: MonadIO m => FontConfig -> CBool -> m ()
setPixelSnapH (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->PixelSnapH = $(bool value);
    }
  |]

-- | Extra spacing (in pixels) between glyphs. Only X axis is supported for now.
--
-- By default, it is @0, 0@
setGlyphExtraSpacing :: MonadIO m => FontConfig -> Ptr ImVec2 -> m ()
setGlyphExtraSpacing (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->GlyphExtraSpacing = *$(ImVec2* value);
    }
  |]

-- | Offset all glyphs from this font input.
--
-- By default, it is @0, 0@
setGlyphOffset :: MonadIO m => FontConfig -> Ptr ImVec2 -> m ()
setGlyphOffset (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->GlyphOffset = *$(ImVec2* value);
    }
  |]

-- | Pointer to a user-provided list of Unicode range (2 value per range, values are inclusive, zero-terminated list). THE ARRAY DATA NEEDS TO PERSIST AS LONG AS THE FONT IS ALIVE.
--
-- By default, it is @NULL@
setGlyphRanges :: MonadIO m => FontConfig -> GlyphRanges -> m ()
setGlyphRanges (FontConfig fc) (GlyphRanges value) = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->GlyphRanges = $(ImWchar* value);
    }
  |]

-- | Minimum AdvanceX for glyphs, set Min to align font icons, set both Min/Max to enforce mono-space font
--
-- By default, it is @0@
setGlyphMinAdvanceX :: MonadIO m => FontConfig -> CFloat -> m ()
setGlyphMinAdvanceX (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->GlyphMinAdvanceX = $(float value);
    }
  |]

-- | Maximum AdvanceX for glyphs
--
-- By default, it is @FLT_MAX@
setGlyphMaxAdvanceX :: MonadIO m => FontConfig -> CFloat -> m ()
setGlyphMaxAdvanceX (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->GlyphMaxAdvanceX = $(float value);
    }
  |]

-- | Merge into previous ImFont, so you can combine multiple inputs font into one ImFont (e.g. ASCII font + icons + Japanese glyphs). You may want to use GlyphOffset.y when merge font of different heights.
--
-- By default, it is @false@
setMergeMode :: MonadIO m => FontConfig -> CBool -> m ()
setMergeMode (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->MergeMode = $(bool value);
    }
  |]

-- | Settings for custom font builder.
-- THIS IS BUILDER IMPLEMENTATION DEPENDENT.
--
-- By default, it is @0@. Leave it so if unsure.
setFontBuilderFlags :: MonadIO m => FontConfig -> CUInt -> m ()
setFontBuilderFlags (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->FontBuilderFlags = $(unsigned int value);
    }
  |]

-- | Brighten (>1.0f) or darken (<1.0f) font output.
-- Brightening small fonts may be a good workaround to make them more readable.
--
-- By default, it is @1.0f@
setRasterizerMultiply :: MonadIO m => FontConfig -> CFloat -> m ()
setRasterizerMultiply (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->RasterizerMultiply = $(float value);
    }
  |]

-- | Explicitly specify unicode codepoint of ellipsis character. When fonts are being merged first specified ellipsis will be used.
--
-- By default, it is @-1@
setEllipsisChar :: MonadIO m => FontConfig -> ImWchar -> m ()
setEllipsisChar (FontConfig fc) value = liftIO do
  [C.block|
    void {
      $(ImFontConfig* fc)->EllipsisChar = $(ImWchar value);
    }
  |]
