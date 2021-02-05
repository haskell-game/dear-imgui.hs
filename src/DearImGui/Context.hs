{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}

module DearImGui.Context where

-- containers
import qualified Data.Map.Strict as Map

-- inline-c
import Language.C.Inline.Context
  ( Context(..) )
import Language.C.Types
  ( pattern TypeName )

-- dear-imgui
import DearImGui.Structs
  ( ImVec2, ImVec3, ImVec4 )

--------------------------------------------------------------------------------

imguiContext :: Context
imguiContext = mempty
  { ctxTypesTable = Map.fromList
      [ ( TypeName "ImVec2", [t| ImVec2 |] )
      , ( TypeName "ImVec3", [t| ImVec3 |] )
      , ( TypeName "ImVec4", [t| ImVec4 |] )
      ]
  }
