{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module DearImGui.Structs
  ( -- * Vectors
    ImVec2(..)
  , ImVec3(..)
  , ImVec4(..)

    -- * Handles
  , Context
  , DrawData
  , DrawList
  , Font
  , FontConfig
  , GlyphRanges
  , ImGuiContext
  , ImDrawData
  , ImDrawList
  , ImFont
  , ImFontConfig
  , ImFontAtlas
  , ImGuiIO
  , ImGuiStyle
  , ImGuiPayload
  , ImGuiListClipper

    -- * Scalars
  , ImGuiID
  , ImU32
  , ImU64
  , ImS16
  , ImWchar
  , ImGuiKeyChord

    -- * Textures
  , ImTextureID
  , ImTextureRef(..)
  , textureRefFromID

    -- * Tables
  , ImGuiTableSortSpecs(..)
  , ImGuiTableColumnSortSpecs(..)
  )
  where

-- base
import Foreign
  ( Storable(..), castPtr, plusPtr, Ptr, nullPtr )

-- dear-imgui-raw
import DearImGui.Raw
  ( ImGuiID, ImU32, ImU64, ImS16, ImWchar, ImTextureID, ImGuiKeyChord )
import DearImGui.Raw.ImDrawData (ImDrawData)
import DearImGui.Raw.ImDrawList (ImDrawList)
import DearImGui.Raw.ImFont (ImFont)
import DearImGui.Raw.ImFontAtlas (ImFontAtlas)
import DearImGui.Raw.ImFontConfig (ImFontConfig)
import DearImGui.Raw.ImGuiContext (ImGuiContext)
import DearImGui.Raw.ImGuiIO (ImGuiIO)
import DearImGui.Raw.ImGuiListClipper (ImGuiListClipper)
import DearImGui.Raw.ImGuiPayload (ImGuiPayload)
import DearImGui.Raw.ImGuiStyle (ImGuiStyle)
import DearImGui.Raw.ImGuiTableColumnSortSpecs (ImGuiTableColumnSortSpecs(..))
import DearImGui.Raw.ImGuiTableSortSpecs (ImGuiTableSortSpecs(..))
import DearImGui.Raw.ImTextureRef (ImTextureRef(..))
import DearImGui.Raw.ImVec2 (ImVec2(..))
import DearImGui.Raw.ImVec4 (ImVec4(..))

-- | DearImGui context handle.
type Context = Ptr ImGuiContext

-- | Draw data produced by 'DearImGui.render', consumed by renderer backends.
type DrawData = Ptr ImDrawData

-- | Draw list handle.
type DrawList = Ptr ImDrawList

-- | Individual font handle.
type Font = Ptr ImFont

-- | Font configuration handle.
type FontConfig = Ptr ImFontConfig

-- | Zero-terminated array of glyph range pairs.
type GlyphRanges = Ptr ImWchar

data ImVec3 = ImVec3 { x, y, z :: {-# unpack #-} !Float }
  deriving (Show)

instance Storable ImVec3 where
  sizeOf ~ImVec3{x, y, z} = sizeOf x + sizeOf y + sizeOf z

  alignment _ = 4

  poke ptr ImVec3{ x, y, z } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z

  peek ptr = do
    x <- peek (castPtr ptr                         )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    return ImVec3{ x, y, z }

-- | Helper to construct `ImTextureRef` from `ImTextureID`
--
-- It follows guidance for binding generators in "What are ImTextureID/ImTextureRef?"
-- see https://github.com/ocornut/imgui/blob/master/docs/FAQ.md#q-what-are-imtextureidimtextureref
textureRefFromID :: ImTextureID -> ImTextureRef
textureRefFromID tid = ImTextureRef { _TexData = nullPtr, _TexID = tid }
