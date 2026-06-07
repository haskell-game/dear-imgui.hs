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

module DearImGui.Raw.Font
  ( -- * Types
    Font(..)
    -- * Adding fonts
  , addFontDefault
  , addFontDefaultVector
  , addFontDefaultBitmap
  , addFontFromFileTTF
  , addFontFromMemoryTTF
    -- * Using fonts
  , pushFont
  , pushFontWithSize
  , pushFontLegacySize
  , pushFontSize
  , popFont

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

-- dear-imgui
import DearImGui.Raw.Context
  ( imguiContext )
import DearImGui.Structs
import DearImGui.Raw.Font.Config
  ( FontConfig(..) )

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

-- | Add the default font to the atlas. Selects between 'AddFontDefaultVector' and 'AddFontDefaultBitmap'.
addFontDefault :: MonadIO m
  => m Font   -- ^ Returns font handle for future usage
addFontDefault = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontDefault();
    }
  |]

-- | Add the embedded scalable vector font. Recommended at any higher size.
addFontDefaultVector :: MonadIO m
  => m Font
addFontDefaultVector = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontDefaultVector();
    }
  |]

-- | Add the embedded classic pixel-clean bitmap font. Recommended at 13px with no scaling.
addFontDefaultBitmap :: MonadIO m
  => m Font
addFontDefaultBitmap = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontDefaultBitmap();
    }
  |]

-- | Add a custom OTF/TTF font from a file.
addFontFromFileTTF :: MonadIO m
  => CString     -- ^ Font file path
  -> CFloat      -- ^ Font size in pixels. Pass `0.0` to let `imgui` pick it automatically. 
  -> FontConfig  -- ^ Configuration data
  -> m Font      -- ^ Returns font handle for future usage
addFontFromFileTTF filenamePtr sizePixels (FontConfig fontConfig) = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontFromFileTTF(
        $(char* filenamePtr),
        $(float sizePixels),
        $(ImFontConfig* fontConfig));
    }
  |]

-- | Transfer a buffer with TTF data to font atlas builder.
-- Pass @0.0@ for size to let `imgui` pick it automatically.
addFontFromMemoryTTF :: MonadIO m => CStringLen -> CFloat -> FontConfig -> m Font
addFontFromMemoryTTF (castPtr -> fontDataPtr, fromIntegral -> fontSize) sizePixels (FontConfig fontConfig) = liftIO do
  Font <$> [C.block|
    ImFont* {
      return GetIO().Fonts->AddFontFromMemoryTTF(
        $(void* fontDataPtr),
        $(int fontSize),
        $(float sizePixels),
        $(ImFontConfig* fontConfig));
    }
  |]


-- | Pushes a font by keeping current size into the parameters stack,
-- so ImGui would render following text using it.
pushFont :: MonadIO m => Font -> m ()
pushFont (Font font) = liftIO do
  [C.exp| void { PushFont($(ImFont* font), 0.0f); } |]

-- | Pushes a font with an explicit size into the parameters stack.
pushFontWithSize :: MonadIO m => Font -> CFloat -> m ()
pushFontWithSize (Font font) size = liftIO do
  [C.exp| void { PushFont($(ImFont* font), $(float size)); } |]

-- | Pushes a font using same size as it was added before into the parameters stack.
pushFontLegacySize :: MonadIO m => Font -> m ()
pushFontLegacySize (Font font) = liftIO do
  [C.exp| void { PushFont($(ImFont* font), $(ImFont* font)->LegacySize); } |]

-- | Keeps current font to change its size only.
pushFontSize :: MonadIO m => CFloat -> m ()
pushFontSize size = liftIO do
  [C.exp| void { PushFont(NULL, $(float size)); } |]

-- | Pops a font pushed into the parameters stack
--
-- Should be called only after a corresponding 'pushFont' call.
popFont :: MonadIO m => m ()
popFont = liftIO do
  [C.exp| void { PopFont(); } |]

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
