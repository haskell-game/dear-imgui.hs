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

{-| Font glyph ranges builder

Helper to build glyph ranges from text/string data.
Feed your application strings/characters to it then call 'buildRanges'.

Low-level example of usage:

@
  -- import ImGui.Fonts
  -- import ImGui.Raw.GlyphRangesBuilder as GRB

  builder <- GRB.new

  GRB.addRanges builder getGlyphRangesDefault
  liftIO $ withCString "Привет" $ GRB.addText builder
  rangesVec <- GRB.buildRanges builder
  let ranges = GRB.fromRangesVector rangesVec

  addFontFromFileTTF'
    "./imgui/misc/fonts/DroidSans.ttf" 12
    Nothing
    (Just ranges)

  -- it is strictly necessary to explicitly build the atlas
  buildFontAtlas

  -- resource destruction comes only after the building
  GRB.destroyRangesVector rangesVec
  GRB.destroy builder
@

-}

module DearImGui.Raw.Font.GlyphRanges
  ( GlyphRanges(..)

    -- * Built-in ranges
  , Builtin(..)
  , getBuiltin
  , builtinSetup

    -- * Preparing a builder
  , GlyphRangesBuilder(..)
  , new
  , destroy
  , addChar
  , addText
  , addRanges

    -- * Extracting data
  , GlyphRangesVector(..)
  , buildRangesVector
  , fromRangesVector
  , destroyRangesVector
  )
  where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign ( Ptr )
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

-- dear-imgui
import DearImGui.Raw.Context
  ( imguiContext )
import DearImGui.Structs

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"

-- | Glyph ranges handle
--
-- Wraps @ImWchar*@.
newtype GlyphRanges = GlyphRanges (Ptr ImWchar)

-- | Builtin glyph ranges tags.
data Builtin
  = Latin
  | Korean
  | Japanese
  | ChineseFull
  | ChineseSimplifiedCommon
  | Cyrillic
  | Thai
  | Vietnamese
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Get builtin glyph ranges from a tag.
getBuiltin :: Builtin -> GlyphRanges
getBuiltin = \case
  Latin                   -> getGlyphRangesDefault
  Korean                  -> getGlyphRangesKorean
  Japanese                -> getGlyphRangesJapanese
  ChineseFull             -> getGlyphRangesChineseFull
  ChineseSimplifiedCommon -> getGlyphRangesChineseSimplifiedCommon
  Cyrillic                -> getGlyphRangesCyrillic
  Thai                    -> getGlyphRangesThai
  Vietnamese              -> getGlyphRangesVietnamese

-- | Special case of @getBuiltin@, but for font source setup.
builtinSetup :: Builtin -> Maybe GlyphRanges
builtinSetup = \case
  Latin -> Nothing
  others  -> Just (getBuiltin others)

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

-- | Glyph ranges builder handle
--
-- Wraps @ImFontGlyphRangesBuilder*@.
newtype GlyphRangesBuilder = GlyphRangesBuilder (Ptr ImFontGlyphRangesBuilder)

-- | Glyph ranges vector handle to keep builder output
--
-- Wraps @ImVector<ImWchar>*@.
newtype GlyphRangesVector = GlyphRangesVector (Ptr ())


-- | Create an instance of builder
new :: MonadIO m => m GlyphRangesBuilder
new = liftIO do
  GlyphRangesBuilder <$> [C.block|
    ImFontGlyphRangesBuilder* {
      return IM_NEW(ImFontGlyphRangesBuilder);
    }
  |]

-- | Destroy an instance of builder
--
-- Should be used __after__ font atlas building.
destroy :: MonadIO m => GlyphRangesBuilder -> m ()
destroy (GlyphRangesBuilder builder) = liftIO do
  [C.block|
    void {
      IM_DELETE($(ImFontGlyphRangesBuilder* builder));
    }
  |]


-- | Add character
addChar :: MonadIO m => GlyphRangesBuilder -> ImWchar -> m ()
addChar (GlyphRangesBuilder builder) wChar = liftIO do
  [C.block|
    void {
      $(ImFontGlyphRangesBuilder* builder)->AddChar($(ImWchar wChar));
    }
  |]

-- | Add string (each character of the UTF-8 string are added)
addText :: MonadIO m => GlyphRangesBuilder -> CString -> m ()
addText (GlyphRangesBuilder builder) string = liftIO do
  [C.block|
    void {
      $(ImFontGlyphRangesBuilder* builder)->AddText($(char* string));
    }
  |]
-- FIXME: the function uses 'const char* text_end = NULL' parameter,
--   which is pointer for the line ending. It is low level, though it
--   could be utilized for string length parameter.

-- | Add ranges, e.g. 'addRanges builder getGlyphRangesDefault'
-- to force add all of ASCII/Latin+Ext
addRanges :: MonadIO m => GlyphRangesBuilder -> GlyphRanges -> m()
addRanges (GlyphRangesBuilder builder) (GlyphRanges ranges) = liftIO do
  [C.block|
    void {
      return;
    }
  |]


-- | Build new ranges and create ranges vector instance,
-- containing them
buildRangesVector :: MonadIO m => GlyphRangesBuilder -> m (GlyphRangesVector)
buildRangesVector (GlyphRangesBuilder builder) = liftIO do
  GlyphRangesVector <$> [C.block|
    void* {
      ImVector<ImWchar>* ranges = IM_NEW(ImVector<ImWchar>);
      return ranges;
    }
  |]

-- | Extract glyph ranges from a vector
--
-- Should be used __before__ vector destruction.
fromRangesVector :: GlyphRangesVector -> GlyphRanges
fromRangesVector (GlyphRangesVector vecPtr) = unsafePerformIO do
  GlyphRanges <$> [C.block|
    ImWchar* {
      return ((ImVector<ImWchar>*) $(void* vecPtr))->Data;
    }
  |]

-- | Destroy a ranges vector instance
--
-- Should be used __after__ font atlas building.
destroyRangesVector :: MonadIO m => GlyphRangesVector -> m ()
destroyRangesVector (GlyphRangesVector vecPtr) = liftIO do
  [C.block|
    void {
      IM_DELETE(((ImVector<ImWchar>*) $(void* vecPtr)));
    }
  |]
