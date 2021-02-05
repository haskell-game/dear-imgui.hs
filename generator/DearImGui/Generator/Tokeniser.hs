{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module DearImGui.Generator.Tokeniser where

-- base
import Control.Arrow
  ( first, second )
import Control.Applicative
  ( (<|>), some )
import Data.Char
  ( isAlpha, isAlphaNum, isDigit, isPunctuation, isSpace, isSymbol, toLower )
import Data.Function
  ( (&) )
import Data.Functor
  ( ($>) )
import Data.Monoid
  ( Sum(..) )

-- megaparsec
import Text.Megaparsec
  ( MonadParsec, VisualStream(..)
  , chunk, parseMaybe, satisfy, try
  )
import Text.Megaparsec.Char.Lexer
  ( hexadecimal, scientific )

-- parser-combinators
import Control.Monad.Combinators
  ( optional )

-- scientific
import Data.Scientific
  ( Scientific )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( break, breakOn, cons, drop, dropWhile
  , head, last, length
  , pack, snoc, span, strip, tail, take
  , uncons, unpack
  )

-- unordered-containers
import Data.HashSet
  ( HashSet )
import qualified Data.HashSet as HashSet
  ( fromList, member )

--------------------------------------------------------------------------------

data TokeniserError
  = Couldn'tParseNumber { problem :: !Text }
  | UnhandledCase { unhandled :: !( Char, Text ) }
  deriving stock ( Eq, Ord, Show )

data Tok
  = Keyword        !Text
  | ReservedSymbol !Char
  | Symbolic       !Text
  | Identifier     !Text
  | Comment        !Text
  | Char           !Char
  | String         !Text
  | Number         !Scientific !Text
  | BeginCPP       !Text
  | EndCPPLine
  deriving stock ( Show, Eq, Ord )

showToken :: Tok -> String
showToken = \case
  Keyword        t -> Text.unpack t
  ReservedSymbol c -> [c]
  Symbolic       t -> Text.unpack t
  Identifier     t -> Text.unpack t
  Comment        t -> Text.unpack t
  Char           c -> [c]
  String         t -> Text.unpack t
  Number       s t -> show s <> Text.unpack t
  BeginCPP       t -> "#" <> Text.unpack t
  EndCPPLine       -> "EndCppLine"

tokenLength :: Tok -> Int
tokenLength = \case
  Keyword        t -> Text.length t
  ReservedSymbol _ -> 1
  Symbolic       t -> Text.length t
  Identifier     t -> Text.length t
  Comment        t -> Text.length t
  Char           _ -> 1
  String         t -> Text.length t
  Number       s t -> length ( show s ) + Text.length t
  BeginCPP       t -> 1 + Text.length t
  EndCPPLine       -> length ( "EndCPPLine" :: String )

instance VisualStream [Tok] where
  showTokens   _ = foldMap showToken
  tokensLength _ = getSum . foldMap ( Sum . tokenLength )

keywords :: HashSet Text
keywords = HashSet.fromList
  [ "auto", "break", "case", "char", "const", "continue", "default", "do", "double"
  , "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long"
  , "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct"
  , "switch", "typedef", "union", "unsigned", "void", "volatile", "while"
  ]

reservedSymbols :: HashSet Char
reservedSymbols = HashSet.fromList [ '(', ')', '{', '}', ',', ';', '=', '#' ]

tokenise :: Text -> Either TokeniserError [ Tok ]
tokenise ( Text.uncons -> Just ( c, cs ) )
  | isSpace c
  = tokenise ( Text.dropWhile isSpace cs )
  | isAlpha c || c == '_'
  , let
      this, rest :: Text
      ( this, rest ) = first ( c `Text.cons` ) $ Text.span ( \ x -> isAlphaNum x || x == '_' ) cs
  = if this `HashSet.member` keywords
    then ( Keyword    this : ) <$> tokenise rest
    else ( Identifier this : ) <$> tokenise rest
  | isDigit c
  , let
      this, rest :: Text
      ( this, rest ) = continuePastExponent $ first ( c `Text.cons` ) $ Text.span ( \ x -> isAlphaNum x || x == '.' ) cs
  = case parseMaybe @() parseNumber this of
      Just numTok -> ( numTok : ) <$> tokenise rest
      Nothing     -> Left ( Couldn'tParseNumber { problem = this } )
  | c == '\''
  , Just ( '\'', rest ) <- Text.uncons ( Text.drop 1 cs )
  = ( Char ( Text.head cs ) : ) <$> tokenise rest
  | c == '\"'
  , let
      this, rest :: Text
      ( this, rest ) = second Text.tail $ Text.break ( == '"') cs
  = ( String this : ) <$> tokenise rest
  | c == '#'
  , let
      directive, line, rest :: Text
      ( directive, ( line, rest ) )
        = cs
        & Text.break ( isSpace )
        & second ( Text.break ( `elem` [ '\n', '\r' ] ) )
  = do
      lineTokens <- tokenise line
      restTokens <- tokenise rest
      pure ( ( BeginCPP directive : lineTokens ) <> ( EndCPPLine : restTokens ) )
  | c `HashSet.member` reservedSymbols
  = ( ReservedSymbol c : ) <$> tokenise cs
  | c == '/'
  = case Text.take 1 cs of
    "/" ->
      let
        comm, rest :: Text
        ( comm, rest ) = first Text.strip $ Text.break ( `elem` [ '\n', '\r' ] ) ( Text.drop 1 cs )
      in ( Comment comm : ) <$> tokenise rest
    "*" ->
      let
        comm, rest :: Text
        ( comm, rest ) = Text.breakOn "*/" ( Text.drop 1 cs )
      in ( Comment comm : ) <$> tokenise rest
    _ ->
      let
        this, rest :: Text
        ( this, rest ) = first ( c `Text.cons` ) $ Text.span ( \ x -> x /= '_' && ( isSymbol x || isPunctuation x ) ) cs
      in ( Symbolic this : ) <$> tokenise rest
  | isSymbol c || isPunctuation c
  , let
      this, rest :: Text
      ( this, rest ) = first ( c `Text.cons` ) $ Text.span ( \ x -> x /= '_' && ( isSymbol x || isPunctuation x ) ) cs
  = ( Symbolic this : ) <$> tokenise rest
  | otherwise
  = Left $ UnhandledCase { unhandled = ( c, cs ) }
tokenise _ = Right []

continuePastExponent :: ( Text, Text ) -> ( Text, Text )
continuePastExponent ( this, rest )
  | toLower ( Text.last this ) `elem` [ 'e', 'p' ]
  , Just ( r, rs ) <- Text.uncons rest
  , r `elem` [ '+', '-' ]
  , ( this', rest' ) <- Text.span isAlphaNum rs
  = ( this `Text.snoc` r <> this', rest' )
  | otherwise
  = ( this, rest )

parseNumber :: MonadParsec e Text m => m Tok
parseNumber = try ( chunk "0.f" $> Number 0 "f" ) <|> do
  value <- try ( chunk "0x" *> hexadecimal ) <|> scientific
  mbSuffix <- fmap ( maybe "" Text.pack ) . optional . some $ satisfy ( \ s -> toLower s `elem` ( "uflz" :: String ) )
  pure ( Number value mbSuffix )
