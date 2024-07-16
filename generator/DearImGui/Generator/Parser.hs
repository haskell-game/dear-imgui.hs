{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module DearImGui.Generator.Parser
  ( CustomParseError(..)
  , headers
  )
  where

-- base
import Control.Applicative
  ( (<|>), many, optional, some )
import Control.Monad
  ( void )
import Data.Bits
  ( Bits(shiftL) )
import Data.Char
  ( isSpace, toLower )
import Data.Either
  ( rights )
import Data.Functor
  ( ($>) )
import Data.Int
  ( Int64 )
import Data.Maybe
  ( catMaybes, fromMaybe )
import Foreign.C.Types
  ( CChar, CInt, CShort, CLLong, CUChar, CUShort, CUInt, CULLong )

-- template-haskell
import qualified Language.Haskell.TH as TH
  ( Name )

-- megaparsec
import Text.Megaparsec
  ( MonadParsec(..), ShowErrorComponent(..)
  , (<?>), anySingle, choice, customFailure, single
  )

-- parser-combinators
import Control.Applicative.Combinators
  ( manyTill, option, sepBy1, skipManyTill )

-- scientific
import Data.Scientific
  ( floatingOrInteger, toBoundedInteger )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( all, any, breakOn, drop, dropWhile, dropWhileEnd
  , length, stripPrefix, unlines, unpack, pack
  )

-- transformers
import Control.Monad.Trans.State.Strict
  ( StateT(..)
  , get, modify'
  )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( fromList, insert, lookup )

-- dear-imgui-generator
import DearImGui.Generator.Tokeniser
  ( Tok(..) )
import DearImGui.Generator.Types
  ( Comment(..), Enumeration(..), Headers(..) )

import qualified Text.Show as Text

--------------------------------------------------------------------------------
-- Parse error type.

data CustomParseError
  = Couldn'tLookupEnumValues
    { enumName :: !Text
    , problems :: ![Text]
    }
  | MissingForwardDeclaration
    { enumName :: !Text
    , library :: HashMap Text ( TH.Name, Comment )
    }
  | UnexpectedSection
    { sectionName :: !Text
    , problem     :: ![Text]
    }
  deriving stock ( Show, Eq, Ord )

instance ShowErrorComponent CustomParseError where
  showErrorComponent ( Couldn'tLookupEnumValues { enumName, problems } ) = Text.unpack $
    "Couldn't lookup the following values in enum " <> enumName <> ":\n"
    <> Text.unlines ( map ( " - "  <> ) problems )
  showErrorComponent ( MissingForwardDeclaration { enumName, library } ) = Text.unpack $
    "Missing forward declaration for enum named " <> enumName <> "\n"
    <> "In Library: " <> Text.pack ( Text.show library)
  showErrorComponent ( UnexpectedSection { sectionName, problem } ) = Text.unpack $
    "Unexpected section name.\n\
    \Expected: " <> sectionName <> "\n\
    \  Actual: " <> Text.unlines ( map ( " "  <> ) problem )

--------------------------------------------------------------------------------
-- Parsing headers.

headers :: MonadParsec CustomParseError [Tok] m => m ( Headers () )
headers = do
  _ <- skipManyTill anySingle ( namedSection "Header mess" )

  _ <- skipManyTill anySingle ( namedSection "Forward declarations" )
  ( _structNames, enumNamesAndTypes ) <- forwardDeclarations

  _ <- skipManyTill anySingle ( namedSection "Dear ImGui end-user API functions" )

  _ <- skipManyTill anySingle ( namedSection "Flags & Enumerations" )

  basicEnums <- rights <$>
    manyTill
      (   ( Left  <$> try ignoreDefine )
      <|> ( Left  <$> try cppConditional )
      <|> ( Right <$> enumeration enumNamesAndTypes )
      )
      ( namedSection "Tables API flags and structures (ImGuiTableFlags, ImGuiTableColumnFlags, ImGuiTableRowFlags, ImGuiTableBgTarget, ImGuiTableSortSpecs, ImGuiTableColumnSortSpecs)" )

  tableEnums <- rights <$>
    manyTill
      (   ( Left  <$> try ignoreDefine )
      <|> ( Left  <$> try cppConditional )
      <|> ( Right <$> enumeration enumNamesAndTypes )
      )
      ( try $ many comment >> keyword "struct" >> identifier)

  _ <- skipManyTill anySingle ( namedSection "Helpers: Memory allocations macros, ImVector<>" )

  _ <- skipManyTill anySingle ( namedSection "ImGuiStyle" )

  _ <- skipManyTill anySingle ( namedSection "ImGuiIO" )

  _ <- skipManyTill anySingle ( namedSection "Misc data structures" )

  _ <- skipManyTill anySingle ( namedSection "Helpers (ImGuiOnceUponAFrame, ImGuiTextFilter, ImGuiTextBuffer, ImGuiStorage, ImGuiListClipper, Math Operators, ImColor)" )

  _ <- skipManyTill anySingle ( namedSection "Drawing API (ImDrawCmd, ImDrawIdx, ImDrawVert, ImDrawChannel, ImDrawListSplitter, ImDrawListFlags, ImDrawList, ImDrawData)" )
  skipManyTill anySingle ( try . lookAhead $ many comment *> keyword "enum" )
  drawingEnums <- many ( enumeration enumNamesAndTypes )

  _ <- skipManyTill anySingle ( namedSection "Font API (ImFontConfig, ImFontGlyph, ImFontAtlasFlags, ImFontAtlas, ImFontGlyphRangesBuilder, ImFont)" )
  skipManyTill anySingle ( try . lookAhead $ many comment *> keyword "enum" )
  fontEnums <- many ( enumeration enumNamesAndTypes )

  _ <- skipManyTill anySingle ( namedSection "Viewports" )

  _ <- skipManyTill anySingle ( namedSection "Platform Dependent Interfaces" ) -- XXX: since 1.87

  _ <- skipManyTill anySingle ( namedSection "Obsolete functions and types" )

  let
    enums :: [ Enumeration () ]
    enums = basicEnums <> tableEnums <> drawingEnums <> fontEnums
  pure ( Headers { enums } )

--------------------------------------------------------------------------------
-- Parsing forward declarations.

forwardDeclarations
  :: MonadParsec CustomParseError [Tok] m
  => m ( HashMap Text Comment, HashMap Text ( TH.Name, Comment ) )
forwardDeclarations = do
  _ <- many comment
  _scalars <- many do
    try $ keyword "typedef"
    signed <- try (keyword "signed" $> True) <|> (keyword "unsigned" $> False)
    width <- try (keyword "int") <|> try (keyword "char") <|> try (keyword "short") <|> try (keyword "int") <|> (keyword "long" >> keyword "long")
    typeName <- identifier
    reservedSymbol ';'
    doc <- comment
    pure (typeName, (signed, width, doc))
  _ <- many comment
  structs <- many do
    keyword "struct"
    structName <- identifier
    reservedSymbol ';'
    doc <- comment
    pure ( structName, doc )
  _ <- many comment
  enums <- many do
    _ <- try do
      _ <- many comment
      cppConditional <|> pure ()
    keyword "enum"
    enumName <- identifier
    symbol ":"
    ty <- cTypeName
    reservedSymbol ';'
    doc <- commentText <$> comment
    pure ( enumName, ( ty, CommentText <$> Text.drop 2 . snd $ Text.breakOn "//" doc ) )
  _ <- many comment
  typedefs <- many do
    keyword "typedef"
    ty <- cTypeName
    enumName <- identifier
    reservedSymbol ';'
    doc <- commentText <$> comment
    _ <- many comment
    pure ( enumName, ( ty, CommentText <$> Text.drop 2 . snd $ Text.breakOn "//" doc ) )
  -- Stopping after simple structs and enums for now.
  pure ( HashMap.fromList structs, HashMap.fromList (enums <> typedefs) )

cTypeName :: MonadParsec e [Tok] m => m TH.Name
cTypeName =
  choice
    [ try $ (keyword "char") $> ''CChar
    , try $ (keyword "signed" >> keyword "int") $> ''CInt
    , try $ (keyword "unsigned" >> keyword "int") $> ''CUInt
    , try $ (keyword "unsigned" >> keyword "char") $> ''CUChar
    , try $ (identifier' "ImS8") $> ''CChar
    , try $ (identifier' "ImU8") $> ''CUChar
    , try $ (identifier' "ImS16") $> ''CShort
    , try $ (identifier' "ImU16") $> ''CUShort
    , try $ (identifier' "ImS32") $> ''CInt
    , try $ (identifier' "ImU32") $> ''CUInt
    , try $ (identifier' "ImS64") $> ''CLLong
    , try $ (identifier' "ImU64") $> ''CULLong
    , keyword "int" $> ''CInt
    ]
  <?> "cTypeName"

--------------------------------------------------------------------------------
-- Parsing enumerations.

data EnumState = EnumState
  { enumValues       :: HashMap Text Integer
  , currEnumTag      :: Integer
  , enumSize         :: Integer
  , hasExplicitCount :: Bool
  }

enumeration :: MonadParsec CustomParseError [Tok] m => HashMap Text ( TH.Name, Comment ) -> m ( Enumeration () )
enumeration enumNamesAndTypes = do
  void $ many (try $ comment >> cppConditional)
  inlineDocs <- try do
    inlineDocs <- many comment
    keyword "enum"
    pure inlineDocs
  fullEnumName <- identifier
  _ <- try $ (symbol ":" >> cTypeName >> pure ()) <|> pure ()
  let
    enumName :: Text
    enumName = Text.dropWhileEnd ( == '_' ) fullEnumName
    enumTypeName :: ()
    enumTypeName = ()
  ( underlyingType, forwardDoc ) <- case HashMap.lookup enumName enumNamesAndTypes of
    Just res -> pure res
    Nothing  -> customFailure ( MissingForwardDeclaration { enumName, library=enumNamesAndTypes } )
  let
    docs :: [Comment]
    docs = forwardDoc : CommentText "" : inlineDocs
  reservedSymbol '{'
  ( patterns, EnumState { enumSize, hasExplicitCount } ) <-
    ( `runStateT` EnumState { enumValues = mempty, currEnumTag = 0, enumSize = 0, hasExplicitCount = False } ) $
      catMaybes
          <$> many
               (   some ignoredPatternContent $> Nothing
               <|> enumerationPattern fullEnumName
               )
  reservedSymbol '}'
  reservedSymbol  ';'
  pure ( Enumeration { .. } )

ignoredPatternContent :: MonadParsec e [Tok] m => m ()
ignoredPatternContent = void ( try comment ) <|> cppConditional

enumerationPattern
  :: MonadParsec CustomParseError [ Tok ] m
  => Text
  -> StateT EnumState m ( Maybe ( Text, Integer, Comment ) )
enumerationPattern enumName = do
  mbPatNameVal <- patternNameAndValue enumName
  _ <- optional $ reservedSymbol ','
  comm <- fromMaybe ( CommentText "" ) <$> optional comment
  pure $
    case mbPatNameVal of
      Nothing                    -> Nothing
      Just ( patName, patValue ) -> Just ( patName, patValue, comm )

patternNameAndValue
  :: forall m
  .  MonadParsec CustomParseError [ Tok ] m
  => Text
  -> StateT EnumState m ( Maybe ( Text, Integer ) )
patternNameAndValue enumName =
  try do
      sz <- count
      modify' ( \ ( EnumState {..} ) -> EnumState { enumSize = sz, hasExplicitCount = True, .. } )
      pure Nothing
  <|> do
        pat@( _, val ) <- value
        modify' ( \ ( EnumState {..} ) -> EnumState { enumSize = enumSize + 1, currEnumTag = val + 1, .. } )
        pure ( Just pat )
  where
    count :: StateT EnumState m Integer
    count = do
      let idName = enumName <> "COUNT"
      _ <- single ( Identifier idName )

      mbVal <- optional do
        _ <- reservedSymbol '='
        EnumState{enumValues} <- get
        integerExpression enumValues

      countVal <- case mbVal of
        Nothing -> currEnumTag <$> get
        Just ct -> pure ct

      modify' ( \ st -> st { enumValues = HashMap.insert idName countVal ( enumValues st ) } )
      pure countVal

    value :: StateT EnumState m ( Text, Integer )
    value = do
      name <- identifier
      val <- patternRHS
      modify' ( \ st -> st { enumValues = HashMap.insert name val ( enumValues st ) } )
      pure ( name, val )
    patternRHS :: StateT EnumState m Integer
    patternRHS =
      ( do
        reservedSymbol '='
        EnumState{enumValues} <- get
        try disjunction <|> try (integerExpression enumValues)
      )
      <|> ( currEnumTag <$> get )

    disjunction :: StateT EnumState m Integer
    disjunction = do
      initial <- identifier <* symbol "|"
      ( rest :: [Text] ) <- identifier `sepBy1` symbol "|"
      let summands = initial : rest
      valsMap <- enumValues <$> get
      let
        res :: Either [ Text ] Integer
        res = foldr
          ( \ summand errsOrVal -> case HashMap.lookup summand valsMap of
            Nothing -> case errsOrVal of { Right _  -> Left [ summand ]; Left errs -> Left ( summand : errs ) }
            Just v  -> case errsOrVal of { Right v' -> Right ( v + v' ); Left errs -> Left errs }
          )
          ( Right 0 )
          summands
      case res of
        Left problems -> customFailure ( Couldn'tLookupEnumValues { enumName, problems } )
        Right v -> pure v

--------------------------------------------------------------------------------
-- Simple token parsers.

comment :: MonadParsec e [ Tok ] m => m Comment
comment = CommentText <$>
  token ( \ case { Comment comm -> Just comm; _ -> Nothing } ) mempty
    <?> "comment"

keyword :: MonadParsec e [ Tok ] m => Text -> m ()
keyword = void . keyword'

keyword' :: MonadParsec e [ Tok ] m => Text -> m Text
keyword' kw = token ( \ case { Keyword kw' | kw == kw' -> Just kw; _ -> Nothing } ) mempty
  <?> ( Text.unpack kw <> " (keyword)" )

identifier :: MonadParsec e [ Tok ] m => m Text
identifier = token ( \ case { Identifier i -> Just i; _ -> Nothing } ) mempty
  <?> "identifier"

identifier' :: MonadParsec e [ Tok ] m => Text -> m Text
identifier' ident = token ( \ case { Identifier i | i == ident -> Just ident; _ -> Nothing } ) mempty
  <?> ( Text.unpack ident <> " (identifier)" )

{-
prefixedIdentifier :: MonadParsec e [ Tok ] m => Text -> m Text
prefixedIdentifier prefix =
  token
    ( \ case
      { Identifier i -> Text.dropWhile ( == '_' ) <$> Text.stripPrefix prefix i
      ; _ -> Nothing
      }
    ) mempty
-}

reservedSymbol :: MonadParsec e [ Tok ] m => Char -> m ()
reservedSymbol s = token ( \ case { ReservedSymbol s' | s == s' -> Just (); _ -> Nothing } ) mempty
  <?> ( [s] <> " (reserved symbol)" )

symbol :: MonadParsec e [ Tok ] m => Text -> m ()
symbol s = token ( \ case { Symbolic s' | s == s' -> Just (); _ -> Nothing } ) mempty
  <?> ( Text.unpack s <> " (symbol)" )

integerExpression :: MonadParsec e [ Tok ] m => HashMap Text Integer -> m Integer
integerExpression enums = try integerPower <|> try integerAdd <|> try integerSub <|> integer
  where
    integerPower :: MonadParsec e [ Tok ] m => m Integer
    integerPower = do
      a <- integer
      _ <- symbol "<<"
      i <- integer
      pure ( a `shiftL` fromIntegral i )

    integerAdd :: MonadParsec e [ Tok ] m => m Integer
    integerAdd = do
      a <- integer
      _ <- symbol "+"
      i <- integer
      pure ( a + i )

    integerSub :: MonadParsec e [ Tok ] m => m Integer
    integerSub = do
      a <- integer
      _ <- symbol "-"
      i <- integer
      pure ( a - i )

    integer :: forall e m. MonadParsec e [ Tok ] m => m Integer
    integer =
      option id mkSign <*>
        token
          ( \case
              Number i suff
                | Just  _  <- toBoundedInteger @Int64 i
                , Right i' <- floatingOrInteger @Float @Integer i
                , not ( Text.any ( (== 'f' ) . toLower ) suff )
                ->
                Just i'

              Identifier name ->
                HashMap.lookup name enums

              _ ->
                Nothing
          )
          mempty
        <?> "integer"
      where
        mkSign :: m ( Integer -> Integer )
        mkSign = ( symbol "+" $> id ) <|> ( symbol "-" $> negate )

section :: MonadParsec e [ Tok ] m => m [Text]
section =
  do
    sectionText <- try do
      separator
      token
        ( \ case
          { Comment txt -> fmap ( Text.dropWhile isSpace )
                         . Text.stripPrefix "[SECTION]"
                         . Text.dropWhile isSpace
                         $ txt
          ; _ -> Nothing
          }
        ) mempty
    rest <- endOfSectionHeader
    pure ( sectionText : filter ( not . Text.all ( \ c -> c == '-' || isSpace c ) ) rest )
  <?> "section"

separator :: MonadParsec e [ Tok ] m => m ()
separator = token
  ( \ case
    { Comment hyphens | Text.length hyphens > 10 && Text.all ( == '-') hyphens -> Just ()
    ; _ -> Nothing
    }
  ) mempty
  <?> "separator"

endOfSectionHeader :: MonadParsec e [ Tok ] m => m [Text]
endOfSectionHeader =  try ( (:) <$> ( commentText <$> comment ) <*> endOfSectionHeader )
                  <|> ( separator $> [] )

namedSection :: MonadParsec CustomParseError [ Tok ] m => Text -> m ()
namedSection sectionName =
  do
    sectionTexts <- section
    case sectionTexts of
      sectionText : _
        | Just _ <- Text.stripPrefix sectionName sectionText
        -> pure ()
      _ -> customFailure ( UnexpectedSection { sectionName, problem = sectionTexts } )
  <?> ( "section named " <> Text.unpack sectionName )

cppDirective :: MonadParsec e [Tok] m => ( Text -> Maybe a ) -> m a
cppDirective f = token ( \case { BeginCPP a -> f a; _ -> Nothing } ) mempty

cppConditional :: MonadParsec e [Tok] m => m ()
cppConditional = do
  void $ cppDirective ( \case { "if" -> Just True; "ifdef" -> Just True; "ifndef" -> Just False; _ -> Nothing } )
  -- assumes no nesting
  void $ skipManyTill anySingle ( cppDirective ( \case { "endif" -> Just (); _ -> Nothing } ) )
  void $ skipManyTill anySingle ( single EndCPPLine )

ignoreDefine :: MonadParsec e [Tok] m => m ()
ignoreDefine = do
  void $ many comment
  void $ cppDirective ( \case { "define" -> Just (); _ -> Nothing } )
  void $ skipManyTill anySingle ( single EndCPPLine )
