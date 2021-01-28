{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module DearImGui.Generator.Types where

-- template-haskell
import qualified Language.Haskell.TH.Syntax as TH
  ( Lift(..), Name(..) )

-- text
import Data.Text
  ( Text )

-- th-lift
import Language.Haskell.TH.Lift
  () -- 'Lift' instance for Name

--------------------------------------------------------------------------------

newtype Comment = CommentText { commentText :: Text }
  deriving stock   ( Show, TH.Lift )
  deriving newtype ( Eq, Ord )

data Enumeration
  = Enumeration
  { docs             :: ![Comment]
  , enumName         :: !Text
  , enumSize         :: !Integer
  , enumType         :: !TH.Name
  , hasExplicitCount :: !Bool
  , patterns         :: [ ( Text, Integer, Comment ) ]
  }
  deriving stock ( Show, TH.Lift )

data Headers
  = Headers
  { enums :: [ Enumeration ] }
  deriving stock ( Show, TH.Lift )
