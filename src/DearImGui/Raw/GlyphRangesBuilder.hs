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
  liftIO $ withCString "Привет" \txt -> GRB.addText builder txt
  rangesVec <- GRB.buildRanges builder
  let ranges = GRB.fromRangesVector rangesVec

  addFontFromFileTTF' "./imgui/misc/fonts/DroidSans.ttf" 12
                      Nothing (Just ranges)

  -- it is strictly necessary to explicitly build the atlas
  buildFontAtlas

  -- resource destruction comes only after the building
  GRB.destroyRangesVector rangesVec
  GRB.destroy builder
@

-}

module DearImGui.Raw.GlyphRangesBuilder
  ( -- * Types
    GlyphRangesBuilder(..)
  , GlyphRangesVector(..)
    -- * Memory management
  , new
  , destroy
  , destroyRangesVector
    -- * Stuffing the builder
  , addChar
  , addText
  , addRanges
    -- * Building and using
  , buildRanges
  , fromRangesVector
  )
  where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign ( Ptr )
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

-- dear-imgui
import DearImGui.Context
  ( imguiContext )
import DearImGui.Structs
import DearImGui.Raw.Fonts

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"


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

-- | Destroy an instance on builder
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
      $(ImFontGlyphRangesBuilder* builder)->AddRanges($(ImWchar* ranges));
    }
  |]


-- | Build new ranges and create ranges vector instance,
-- containing them
buildRanges :: MonadIO m => GlyphRangesBuilder -> m (GlyphRangesVector)
buildRanges (GlyphRangesBuilder builder) = liftIO do
  GlyphRangesVector <$> [C.block|
    void* {
      ImVector<ImWchar>* ranges = IM_NEW(ImVector<ImWchar>);
      $(ImFontGlyphRangesBuilder* builder)->BuildRanges(ranges);
      return ranges;
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
