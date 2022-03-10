{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module DearImGui.Structs where

-- base
import Data.Word
  ( Word32
#ifndef IMGUI_USE_WCHAR32
  , Word16
#endif
  )

import Foreign
  ( Storable(..), castPtr, plusPtr, Ptr, Int16 )
import Foreign.C
  ( CInt, CBool )

import DearImGui.Enums

--------------------------------------------------------------------------------
data ImVec2 = ImVec2 { x, y :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec2 where
  sizeOf ~ImVec2{x, y} = sizeOf x + sizeOf y

  alignment _ = 0

  poke ptr ImVec2{ x, y } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    return ImVec2{ x, y  }


data ImVec3 = ImVec3 { x, y, z :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec3 where
  sizeOf ~ImVec3{x, y, z} = sizeOf x + sizeOf y + sizeOf z

  alignment _ = 0

  poke ptr ImVec3{ x, y, z } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    return ImVec3{ x, y, z }


data ImVec4 = ImVec4 { x, y, z, w :: {-# unpack #-} !Float }
  deriving (Show)


instance Storable ImVec4 where
  sizeOf ~ImVec4{x, y, z, w} = sizeOf x + sizeOf y + sizeOf z + sizeOf w

  alignment _ = 0

  poke ptr ImVec4{ x, y, z, w } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z
    poke (castPtr ptr `plusPtr` (sizeOf x * 3)) w

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    w <- peek (castPtr ptr `plusPtr` (sizeOf x * 3))
    return ImVec4{ x, y, z, w }

--------------------------------------------------------------------------------

-- | DearImGui context handle.
data ImGuiContext

-- | Individual font handle.
data ImFont

-- | Font configuration handle.
data ImFontConfig

-- | Glyph ranges builder handle.
data ImFontGlyphRangesBuilder

-- | Opaque DrawList handle.
data ImDrawList

-- | 'DearImGui.Raw.ListClipper.ListClipper' pointer tag.
data ImGuiListClipper

-- | A unique ID used by widgets (typically the result of hashing a stack of string)
--   unsigned Integer (same as ImU32)
type ImGuiID = Word32

-- | 32-bit unsigned integer (often used to store packed colors).
type ImU32 = Word32

type ImS16 = Int16

-- | Single wide character (used mostly in glyph management)
#ifdef IMGUI_USE_WCHAR32
type ImWchar = Word32
#else
type ImWchar = Word16
#endif

--------------------------------------------------------------------------------

-- | Sorting specifications for a table (often handling sort specs for a single column, occasionally more)
--   Obtained by calling TableGetSortSpecs().
--   When @SpecsDirty == true@ you can sort your data. It will be true with sorting specs have changed since last call, or the first time.
--   Make sure to set @SpecsDirty = false@ after sorting, else you may wastefully sort your data every frame!
data ImGuiTableSortSpecs = ImGuiTableSortSpecs
     { imGuiTableColumnSortSpecs :: Ptr ImGuiTableColumnSortSpecs
     , imGuiTableSortSpecsCount :: CInt
     , imGuiTableSortSpecsDirty :: CBool
     }

instance Storable ImGuiTableSortSpecs where
  sizeOf _ = sizeOf (undefined :: Ptr ImGuiTableColumnSortSpecs)
           + sizeOf (undefined :: CInt)
           + sizeOf (undefined :: CBool)

  alignment _ = 0

  poke ptr (ImGuiTableSortSpecs s c d) = do
    poke ( castPtr ptr                   ) s
    poke ( castPtr ptr `plusPtr` sizeOf s) c
    poke ((castPtr ptr `plusPtr` sizeOf s)
                       `plusPtr` sizeOf c) d

  peek ptr = do
    s <- peek ( castPtr ptr                   )
    c <- peek ( castPtr ptr `plusPtr` sizeOf s)
    d <- peek ((castPtr ptr `plusPtr` sizeOf s)
                            `plusPtr` sizeOf c)
    return (ImGuiTableSortSpecs s c d)

-- | Sorting specification for one column of a table
data ImGuiTableColumnSortSpecs = ImGuiTableColumnSortSpecs
     { imGuiTableColumnSortUserID      :: ImGuiID -- ^ User id of the column (if specified by a TableSetupColumn() call)
     , imGuiTableColumnSortColumnIndex :: ImS16 -- ^ Index of the column
     , imGuiTableColumnSortOrder       :: ImS16 -- ^ Index within parent ImGuiTableSortSpecs (always stored in order starting from 0, tables sorted on a single criteria will always have a 0 here)
     , imGuiTableColumnSortDirection   :: ImGuiSortDirection -- ^ 'ImGuiSortDirection_Ascending' or 'ImGuiSortDirection_Descending'
     } deriving (Show, Eq)

instance Storable ImGuiTableColumnSortSpecs where
  sizeOf _  = sizeOf (undefined :: ImGuiID)
            + sizeOf (undefined :: ImS16)
            + sizeOf (undefined :: ImS16)
            + sizeOf (undefined :: ImGuiSortDirection)

  alignment _ = 0

  poke ptr (ImGuiTableColumnSortSpecs a b c d) = do
    poke (  castPtr ptr                   ) a
    poke (  castPtr ptr `plusPtr` sizeOf a) b
    poke (( castPtr ptr `plusPtr` sizeOf a)
                        `plusPtr` sizeOf b) c
    poke (((castPtr ptr `plusPtr` sizeOf a)
                        `plusPtr` sizeOf b)
                        `plusPtr` sizeOf c) d

  peek ptr = do
    a <- peek (  castPtr ptr                   )
    b <- peek (  castPtr ptr `plusPtr` sizeOf a)
    c <- peek (( castPtr ptr `plusPtr` sizeOf a)
                             `plusPtr` sizeOf b)
    d <- peek (((castPtr ptr `plusPtr` sizeOf a)
                             `plusPtr` sizeOf b)
                             `plusPtr` sizeOf c)
    return (ImGuiTableColumnSortSpecs a b c d)
