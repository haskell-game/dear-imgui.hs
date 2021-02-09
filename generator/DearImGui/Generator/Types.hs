{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module DearImGui.Generator.Types where

-- base
import Data.Functor
  ( (<&>) )

-- template-haskell
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( unpack )

-- th-lift
import Language.Haskell.TH.Lift
  () -- 'Lift' instance for Name

--------------------------------------------------------------------------------

newtype Comment = CommentText { commentText :: Text }
  deriving stock   ( Show, TH.Lift )
  deriving newtype ( Eq, Ord )

data Enumeration typeName
  = Enumeration
  { docs             :: ![Comment]
  , enumName         :: !Text
  , enumTypeName     :: !typeName
  , enumSize         :: !Integer
  , underlyingType   :: !TH.Name
  , hasExplicitCount :: !Bool
  , patterns         :: [ ( Text, Integer, Comment ) ]
  }
  deriving stock ( Show, TH.Lift )

data Headers typeName
  = Headers
  { enums :: [ Enumeration typeName ] }
  deriving stock ( Show, TH.Lift )

generateNames :: Headers () -> Headers ( TH.Name, TH.Name )
generateNames ( Headers { enums = basicEnums } ) = Headers { enums = namedEnums }
  where
    namedEnums :: [ Enumeration ( TH.Name, TH.Name ) ]
    namedEnums = basicEnums <&> \ enum@( Enumeration { enumName } ) ->
      let
        enumNameStr :: String
        enumNameStr = Text.unpack enumName
        tyName, conName :: TH.Name
        tyName  = TH.mkName enumNameStr
        conName = TH.mkName enumNameStr
      in
        enum { enumTypeName = ( tyName, conName ) }
