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
import DearImGui.Enums
import DearImGui.Structs

--------------------------------------------------------------------------------

imguiContext :: Context
imguiContext = mempty
  { ctxTypesTable = Map.fromList
      [ ( TypeName "ImGuiCol" , [t| ImGuiCol |] )
      , ( TypeName "ImGuiCond", [t| ImGuiCond |] )
      , ( TypeName "ImGuiDir" , [t| ImGuiDir |] )
      , ( TypeName "ImGuiStyleVar"    , [t| ImGuiStyleVar |] )
      , ( TypeName "ImGuiTabBarFlags" , [t| ImGuiTabBarFlags |] )
      , ( TypeName "ImGuiTabItemFlags", [t| ImGuiTabItemFlags |] )
      , ( TypeName "ImVec2", [t| ImVec2 |] )
      , ( TypeName "ImVec3", [t| ImVec3 |] )
      , ( TypeName "ImVec4", [t| ImVec4 |] )
      ]
  }
