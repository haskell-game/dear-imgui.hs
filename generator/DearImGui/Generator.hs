{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module DearImGui.Generator
  ( declareEnumerations )
  where

-- base
import Data.Coerce
  ( coerce )
import Data.Bits
  ( Bits )
import Data.Foldable
  ( toList )
import Data.Traversable
  ( for )

-- directory
import System.Directory
  ( canonicalizePath )

-- megaparsec
import qualified Text.Megaparsec as Megaparsec
  ( ParseErrorBundle(bundleErrors), parse, parseErrorPretty )

-- template-haskell
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-- text
import qualified Data.Text as Text
  ( isInfixOf, null, unpack, unlines )
import qualified Data.Text.IO as Text
  ( readFile )

-- dear-imgui-generator
import qualified DearImGui.Generator.Parser as Parser
  ( headers )
import DearImGui.Generator.Tokeniser
  ( tokenise )
import DearImGui.Generator.Types
  ( Comment(..), Enumeration(..), Headers(..) )
import Paths_dear_imgui
  ( getDataFileName )

--------------------------------------------------------------------------------
-- Obtaining parsed header data.

headers :: Headers
headers = $( TH.lift =<< TH.runIO do
  headersPath <- canonicalizePath =<< getDataFileName "imgui/imgui.h"
  headersSource <- Text.readFile headersPath
  tokens <- case tokenise headersSource of
    Left  err  -> error ( "Couldn't tokenise Dear ImGui headers:\n\n" <> show err )
    Right toks -> pure toks
  case Megaparsec.parse Parser.headers "" tokens of
    Left  err -> error $
      "Couldn't parse Dear ImGui headers:\n\n" <>
      ( unlines ( map Megaparsec.parseErrorPretty . toList $ Megaparsec.bundleErrors err ) )
    Right res -> pure res
  )

--------------------------------------------------------------------------------
-- Generating TH splices.

declareEnumerations :: TH.Q [ TH.Dec ]
declareEnumerations = concat <$> mapM declareEnumeration ( enums headers )

declareEnumeration :: Enumeration -> TH.Q [ TH.Dec ]
declareEnumeration ( Enumeration {..} ) = do
  let
    enumNameStr :: String
    enumNameStr = Text.unpack enumName
    isFlagEnum :: Bool
    isFlagEnum = "Flags" `Text.isInfixOf` enumName
  tyName  <- TH.newName enumNameStr

  conName <- TH.newName enumNameStr
  let
    newtypeCon :: TH.Q TH.Con
    newtypeCon =
      TH.normalC conName
        [ TH.bangType
          ( TH.bang TH.noSourceUnpackedness TH.noSourceStrictness )
          ( TH.conT enumType )
        ]
    classes :: [ TH.Q TH.Type ]
    classes
      | isFlagEnum
      = map TH.conT [ ''Eq, ''Ord, ''Bits ]
      | otherwise
      = map TH.conT [ ''Eq, ''Ord ]
    derivClause :: TH.Q TH.DerivClause
    derivClause = TH.derivClause ( Just TH.NewtypeStrategy ) classes
  newtypeDecl <-
#if MIN_VERSION_base(4,16,0)
    ( if   null docs
      then TH.newtypeD
      else
        \ ctx name bndrs kd con derivs ->
          TH.newtypeD_doc ctx name ( fmap pure bndrs ) ( fmap pure kd ) ( con, "", [] ) derivs
            ( Text.unpack . Text.unlines . coerce $ docs )
    )
#else
    TH.newtypeD
#endif
    ( pure [] ) tyName [] Nothing newtypeCon [ derivClause ]

  -- TODO: define the size of the enum for those with explicit count, e.g.
  -- instance Finitary EnumName where
  --   type Cardinality EnumName = enumSize
  --   fromFinite a = EnumCon ( fromIntegral a )
  --   toFinite ( EnumCon a ) = fromIntegral a
  
  synonyms <- for patterns \ ( patternName, patternValue, CommentText patDoc ) -> do
    let
      patNameStr :: String
      patNameStr = Text.unpack patternName
    patName   <- TH.newName patNameStr
    patSynSig <- TH.patSynSigD patName ( TH.conT tyName )
    pat       <-
#if MIN_VERSION_base(4,16,0)
      ( if   Text.null patDoc
        then TH.patSynD
        else
          \ nm args dir pat ->
          TH.patSynD_doc nm args dir pat
            ( Text.unpack patDoc ) []
      )
#else
      TH.patSynD
#endif
        patName ( TH.prefixPatSyn [] ) TH.implBidir
        ( TH.conP conName [ TH.litP $ TH.integerL patternValue ] )
    pure ( patSynSig, pat )

  pure ( newtypeDecl : unpairs synonyms )

unpairs :: [ ( a, a ) ] -> [ a ]
unpairs [] = []
unpairs ( ( x, y ) : as ) = x : y : unpairs as
