{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module DearImGui.Context where

import Language.C.Types
import Language.C.Inline.Context
import qualified Data.Map.Strict as Map
import Foreign


data ImVec4 = ImVec4 { x, y, z, w :: {-# unpack #-} Float }


instance Storable ImVec4 where
  sizeOf ~ImVec4{x, y, z, w} = sizeOf x + sizeOf y + sizeOf z + sizeOf w

  alignment _ = 0

  poke ptr ImVec4{ x, y, z, w } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` (sizeOf x * 1)) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z
    poke (castPtr ptr `plusPtr` (sizeOf x * 3)) w

  peek ptr = do
    x <- peek (castPtr ptr `plusPtr`               )
    y <- peek (castPtr ptr `plusPtr` (sizeOf x * 1))
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    w <- peek (castPtr ptr `plusPtr` (sizeOf x * 3))
    return ImVec4{ x, y, z, w }


imguiContext :: Context
imguiContext = mempty
  { ctxTypesTable = Map.fromList
      [ ( TypeName "ImVec4", [t| ImVec4 |] )
      ]
    }
