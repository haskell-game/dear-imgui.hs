{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module DearImGui.Generator
  ( declareEnumerations, enumerationsTypesTable )
  where

-- base
import Control.Arrow
  ( second )
import Data.Bits
  ( Bits )
import Data.Foldable
  ( toList )
import qualified Data.List.NonEmpty as NonEmpty
  ( head )
import Data.String
  ( fromString )
import Data.Traversable
  ( for )
import Foreign.Storable
  ( Storable )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( fromList )

-- directory
import System.Directory
  ( canonicalizePath )

-- filepath
import System.FilePath
  ( takeDirectory )

-- inline-c
import qualified Language.C.Types as InlineC
  ( TypeSpecifier(TypeName) )

-- megaparsec
import qualified Text.Megaparsec as Megaparsec

-- template-haskell
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-- text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
  ( readFile )

-- dear-imgui-generator
import qualified DearImGui.Generator.Parser as Parser
  ( headers )
import DearImGui.Generator.Tokeniser
  ( Tok, tokenise )
import DearImGui.Generator.Types
  ( Comment(..), Enumeration(..), Headers(..)
  , generateNames
  )

--------------------------------------------------------------------------------
-- Obtaining parsed header data.

headers :: Headers ( TH.Name, TH.Name )
headers = $( do
  currentPath <- TH.loc_filename <$> TH.location
  basicHeaders <- TH.runIO do
    headersPath  <- canonicalizePath ( takeDirectory currentPath <> "/../../imgui/imgui.h" )
    headersSource <- Text.readFile headersPath
    tokens <- case tokenise headersSource of
      Left  err  -> error ( "Couldn't tokenise Dear ImGui headers:\n\n" <> show err )
      Right toks -> pure toks
    case Megaparsec.parse Parser.headers "" tokens of
      Left  err -> do
        let
          errorPos :: Int
          errorPos = Megaparsec.errorOffset . NonEmpty.head $ Megaparsec.bundleErrors err
          prev, rest :: [ Tok ]
          ( prev, rest ) = second ( take 15 ) . splitAt 5 . drop ( errorPos - 5 ) $ tokens
        error $
          "Couldn't parse Dear ImGui headers:\n\n" <>
          ( unlines ( map Megaparsec.parseErrorPretty . toList $ Megaparsec.bundleErrors err ) ) <> "\n" <>
          ( unlines ( map show prev ) <> "\n\n" <> unlines ( map show rest ) )
      Right res -> pure res
  TH.lift $ generateNames basicHeaders
  )

--------------------------------------------------------------------------------
-- Generating TH splices.

enumerationsTypesTable :: Map InlineC.TypeSpecifier ( TH.Q TH.Type )
enumerationsTypesTable = Map.fromList . map mkTypePair $ enums headers
  where
    mkTypePair :: Enumeration ( TH.Name, TH.Name ) -> ( InlineC.TypeSpecifier, TH.Q TH.Type )
    mkTypePair ( Enumeration { enumName, enumTypeName } ) =
      ( InlineC.TypeName $ fromString ( Text.unpack enumName )
      , TH.conT ( fst $ enumTypeName )
      )

declareEnumerations :: TH.Name -> TH.Name -> TH.Q [ TH.Dec ]
declareEnumerations finiteEnumName countName = do
  concat <$> mapM ( declareEnumeration finiteEnumName countName ) ( enums headers )

declareEnumeration :: TH.Name -> TH.Name -> Enumeration ( TH.Name, TH.Name ) -> TH.Q [ TH.Dec ]
declareEnumeration finiteEnumName countName ( Enumeration {..} ) = do
  let
    tyName, conName :: TH.Name
    ( tyName, conName ) = enumTypeName
    isFlagEnum :: Bool
    isFlagEnum = "Flags" `Text.isInfixOf` enumName
    newtypeCon :: TH.Q TH.Con
    newtypeCon =
      TH.normalC conName
        [ TH.bangType
          ( TH.bang TH.noSourceUnpackedness TH.noSourceStrictness )
          ( TH.conT underlyingType )
        ]
    classes :: [ TH.Q TH.Type ]
    classes
      | isFlagEnum
      = map TH.conT [ ''Eq, ''Ord, ''Show, ''Storable, ''Bits ]
      | otherwise
      = map TH.conT [ ''Eq, ''Ord, ''Show, ''Storable ]
    derivClause :: TH.Q TH.DerivClause
    derivClause = TH.derivClause ( Just TH.NewtypeStrategy ) classes

  newtypeDecl <-
#if MIN_VERSION_template_haskell(2,18,0)
    ( if   null docs
      then TH.newtypeD
      else
        \ ctx name bndrs kd con derivs ->
          TH.newtypeD_doc ctx name ( fmap pure bndrs ) ( fmap pure kd ) ( con, Nothing, [] ) derivs
            ( Just . Text.unpack . Text.unlines . coerce $ docs )
    )
#else
    TH.newtypeD
#endif
    ( pure [] ) tyName [] Nothing newtypeCon [ derivClause ]

  mbAddFiniteEnumInst <-
    if hasExplicitCount
    then do
      finiteEnumInst <-
        TH.instanceD ( pure [] ) ( TH.appT ( TH.conT finiteEnumName ) ( TH.conT tyName ) )
          [ TH.tySynInstD ( TH.TySynEqn Nothing
                        <$> TH.appT ( TH.conT countName ) ( TH.conT tyName )
                        <*> TH.litT ( TH.numTyLit enumSize )
                          )
          ]
      pure ( finiteEnumInst : )
    else pure id

  synonyms <- for patterns \ ( patternName, patternValue, CommentText _patDoc ) -> do
    let
      patNameStr :: String
      patNameStr = Text.unpack patternName
    patName   <- TH.newName patNameStr
    patSynSig <- TH.patSynSigD patName ( TH.conT tyName )
    pat       <-
#if MIN_VERSION_template_haskell(2,18,0)
      ( if   Text.null _patDoc
        then TH.patSynD
        else
          \ nm args dir pat ->
          TH.patSynD_doc nm args dir pat
            ( Just $ Text.unpack patDoc ) []
      )
#else
      TH.patSynD
#endif
        patName ( TH.prefixPatSyn [] ) TH.implBidir
        ( TH.conP conName [ TH.litP $ TH.integerL patternValue ] )
    pure ( patSynSig, pat )

  pure ( newtypeDecl : mbAddFiniteEnumInst ( unpairs synonyms ) )

unpairs :: [ ( a, a ) ] -> [ a ]
unpairs [] = []
unpairs ( ( x, y ) : as ) = x : y : unpairs as
