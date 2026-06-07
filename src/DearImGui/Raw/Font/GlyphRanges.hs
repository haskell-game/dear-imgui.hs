{-# LANGUAGE TemplateHaskell #-}

{-| Glyph ranges

-}

module DearImGui.Raw.Font.GlyphRanges
  ( GlyphRanges(..)
  )
  where

-- base
import Foreign ( Ptr )

-- dear-imgui
import DearImGui.Structs
  ( ImWchar )

-- | Glyph ranges handle
--
-- Wraps @ImWchar*@.
newtype GlyphRanges = GlyphRanges (Ptr ImWchar)
