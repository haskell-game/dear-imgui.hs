{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module DearImGui.Structs where

-- base
import Foreign
  ( Storable(..), castPtr, plusPtr )

--------------------------------------------------------------------------------
data ImVec2 = ImVec2 { x, y :: {-# unpack #-} !Float } deriving (Show)


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
