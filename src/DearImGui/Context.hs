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

-- dear-imgui-generator
import DearImGui.Generator
  ( enumerationsTypesTable )

--------------------------------------------------------------------------------

imguiContext :: Context
imguiContext = mempty
  { ctxTypesTable = enumerationsTypesTable <>
    Map.fromList
      [ ( TypeName "ImVec2", [t| ImVec2 |] )
      , ( TypeName "ImVec3", [t| ImVec3 |] )
      , ( TypeName "ImVec4", [t| ImVec4 |] )
      , ( TypeName "ImU32", [t| ImU32 |] )
      , ( TypeName "ImWchar", [t| ImWchar |] )
      , ( TypeName "ImDrawList", [t| ImDrawList |] )
      , ( TypeName "ImGuiContext", [t| ImGuiContext |] )
      , ( TypeName "ImFont", [t| ImFont |] )
      , ( TypeName "ImFontConfig", [t| ImFontConfig |] )
      , ( TypeName "ImGuiListClipper", [t| ImGuiListClipper |] )
      ]
  }
