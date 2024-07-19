{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
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
  ( Storable(..), castPtr, plusPtr, Ptr, Int16, nullPtr )
import Foreign.C
  ( CInt, CBool )

import DearImGui.Enums
import Data.Bits ((.&.))

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

-- | 'DearImGui.Raw.DragDrop.Payload' pointer tag.
data ImGuiPayload

-- | A unique ID used by widgets (typically the result of hashing a stack of string)
--   unsigned Integer (same as ImU32)
type ImGuiID = ImU32

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
  { specs      :: Ptr ImGuiTableColumnSortSpecs
  , specsCount :: CInt
  , specsDirty :: CBool
  } deriving (Show, Eq)

instance Storable ImGuiTableSortSpecs where
  sizeOf _ =
    sizeOf (undefined :: Ptr ImGuiTableColumnSortSpecs) +
    sizeOf (undefined :: CInt) +
    sizeOf (undefined :: CBool)

  alignment _ =
    alignment nullPtr

  poke ptr ImGuiTableSortSpecs{..} = do
    let specsPtr = castPtr ptr
    poke specsPtr specs

    let specsCountPtr = castPtr $ specsPtr `plusPtr` sizeOf specs
    poke specsCountPtr specsCount

    let specsDirtyPtr = castPtr $ specsCountPtr `plusPtr` sizeOf specsCount
    poke specsDirtyPtr specsDirty

  peek ptr = do
    let specsPtr = castPtr ptr
    specs <- peek specsPtr

    let specsCountPtr = castPtr $ specsPtr `plusPtr` sizeOf specs
    specsCount <- peek specsCountPtr

    let specsDirtyPtr = castPtr $ specsCountPtr `plusPtr` sizeOf specsCount
    specsDirty <- peek specsDirtyPtr

    pure ImGuiTableSortSpecs{..}

-- | Sorting specification for one column of a table
data ImGuiTableColumnSortSpecs = ImGuiTableColumnSortSpecs
  { columnUserID  :: ImGuiID            -- ^ User id of the column (if specified by a TableSetupColumn() call)
  , columnIndex   :: ImS16              -- ^ Index of the column
  , sortOrder     :: ImS16              -- ^ Index within parent ImGuiTableSortSpecs (always stored in order starting from 0, tables sorted on a single criteria will always have a 0 here)
  , sortDirection :: ImGuiSortDirection -- ^ 'ImGuiSortDirection_Ascending' or 'ImGuiSortDirection_Descending'
  } deriving (Show, Eq)

instance Storable ImGuiTableColumnSortSpecs where
  sizeOf _ = 12
  alignment _ = 4

  poke ptr ImGuiTableColumnSortSpecs{..} = do
    let columnUserIDPtr = castPtr ptr
    poke columnUserIDPtr columnUserID

    let columnIndexPtr = castPtr $ columnUserIDPtr `plusPtr` sizeOf columnUserID
    poke columnIndexPtr columnIndex

    let sortOrderPtr = castPtr $ columnIndexPtr `plusPtr` sizeOf columnIndex
    poke sortOrderPtr sortOrder

    let sortDirectionPtr = castPtr $ sortOrderPtr `plusPtr` sizeOf sortOrder
    poke sortDirectionPtr sortDirection

  peek ptr = do
    let columnUserIDPtr = castPtr ptr
    columnUserID <- peek columnUserIDPtr

    let columnIndexPtr = castPtr $ columnUserIDPtr `plusPtr` sizeOf columnUserID
    columnIndex <- peek columnIndexPtr

    let sortOrderPtr = castPtr $ columnIndexPtr `plusPtr` sizeOf columnIndex
    sortOrder <- peek sortOrderPtr

    let sortDirectionPtr = castPtr $ sortOrderPtr `plusPtr` sizeOf sortOrder
    sortDirection' <- peek sortDirectionPtr :: IO CInt
    -- XXX: Specs struct uses trimmed field: @SortDirection : 8@
    let sortDirection = case sortDirection' .&. 0xFF of
          0 ->
            ImGuiSortDirection_None
          1 ->
            ImGuiSortDirection_Ascending
          2 ->
            ImGuiSortDirection_Descending
          _ ->
            error $ "Unexpected value for ImGuiSortDirection: " <> show sortDirection

    pure ImGuiTableColumnSortSpecs{..}
