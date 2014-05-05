{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, ImpredicativeTypes,
             DeriveDataTypeable #-}
module Eyeshadow.Phase.Lexical.Types
  (Action,
   Classification(..),
   LexerStateData(..),
   LexerStateDataActionMap(..),
   LexerState(..),
   Lexer(..))
  where

import qualified Control.Eff as Eff
import qualified Control.Eff.Reader.Strict as Eff
import qualified Control.Eff.State.Strict as Eff
import qualified Data.Conduit as Conduit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Data.Char
import Data.Maybe
import Data.Typeable

import Eyeshadow.Data.Span
import Eyeshadow.Data.Token
import Eyeshadow.Diagnostic
import Eyeshadow.Prelude


type Action a =
  (Eff.Member Diagnose r,
   Eff.Member (Eff.Reader Lexer) r,
   Eff.Member (Eff.State LexerState) r)
  => Conduit.ConduitM (Char, Span) Token (Eff.Eff r) a


data Classification
  = LetterClassification
  | NumberClassification
  | OperatorClassification
  | HorizontalWhitespaceClassification
  | VerticalWhitespaceClassification
  | QuoteClassification
  | ApostropheClassification
  | PlusClassification
  | CommaClassification
  | MinusClassification
  | PeriodClassification
  | ColonClassification
  | SemicolonClassification
  | AtSignClassification
  | BackslashClassification
  | BacktickClassification
  | OpenParenthesisClassification
  | CloseParenthesisClassification
  | ConnectingPunctuationClassification
  | UnknownClassification
  deriving (Eq, Ord)


data LexerStateData
  = TopLevelLexerStateData
  | WordLexerStateData
  | NumberLexerStateData
  | OperatorLexerStateData
  | PeriodLexerStateData
  | PlusLexerStateData
  | MinusLexerStateData
  | HyphenLexerStateData
  | DashLexerStateData
  | StringLexerStateData
  | OpenParenthesisLexerStateData
  | CloseParenthesisLexerStateData
  | CommaLexerStateData
  | ColonLexerStateData
  | SemicolonLexerStateData
  | SpliceLexerStateData
  | ListSpliceLexerStateData
  | WhitespaceLexerStateData
  deriving (Eq, Ord)


data LexerStateDataActionMap =
  LexerStateDataActionMap {
      lexerStateDataActionMapClassificationActionMap
        :: HashMap.HashMap Classification (Action ()),
      lexerStateDataActionMapEndAction
        :: Maybe (Action ()),
      lexerStateDataActionMapDefaultAction
        :: Action ()
    }


data LexerState =
  LexerState {
      lexerStateSpan :: Maybe Span,
      lexerStateAccumulator :: Text.Text,
      lexerStateValue :: Maybe Text.Text,
      lexerStateOpenDelimiter :: Maybe Char,
      lexerStateCloseDelimiter :: Maybe Char,
      lexerStateData :: LexerStateData,
      lexerStateInput :: Maybe (Char, Span, Classification),
      lexerStateDone :: Bool
    }
  deriving (Typeable)


data Lexer =
  Lexer {
      lexerClassificationMap :: HashMap.HashMap Char Classification,
      lexerMinorCategoryMap :: HashMap.HashMap Text.Text Classification,
      lexerMajorCategoryMap :: HashMap.HashMap Text.Text Classification,
      lexerQuoteMap :: HashMap.HashMap Char [Char],
      lexerParenthesisMap :: HashMap.HashMap Char [Char],
      lexerEscapeMap :: HashMap.HashMap Char Char,
      lexerActionMap :: HashMap.HashMap LexerStateData LexerStateDataActionMap
    }
  deriving (Typeable)
