{-# LANGUAGE OverloadedStrings #-}
module Lexer
  (Position(..),
   Span(..),
   Token(..),
   Classification(..),
   LexerStateData(..),
   LexerStateDataActionMap(..),
   LexerState,
   Lexer(..),
   defaultClassificationMap,
   defaultMinorCategoryMap,
   defaultMajorCategoryMap,
   defaultQuoteMap,
   defaultParenthesisMap,
   defaultEscapeMap,
   defaultActionMap,
   defaultLexer,
   classify,
   runLexer)
  where

import Prelude hiding (Show(..))

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Conduit
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Error
import Knapp.Show
import Unicode


data TokenType = TokenType
data Token =
  Token {
      tokenType :: TokenType,
      tokenSpan :: Span,
      tokenText :: Text
    }
instance Show Token where
  show token = "<token>"


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
  | EscapeSequenceLexerStateData
  | OpenParenthesisLexerStateData
  | CloseParenthesisLexerStateData
  | PreCommaLexerStateData
  | CommaLexerStateData
  | PreColonLexerStateData
  | ColonLexerStateData
  | PreSemicolonLexerStateData
  | SemicolonLexerStateData
  | SpliceLexerStateData
  | ListSpliceLexerStateData
  | WhitespaceLexerStateData
  deriving (Eq, Ord)


data LexerStateDataActionMap =
  LexerStateDataActionMap {
      lexerStateDataActionMapClassificationActionMap
        :: Map Classification (LexerMonad ()),
      lexerStateDataActionMapEndAction
        :: Maybe (LexerMonad ()),
      lexerStateDataActionMapDefaultAction
        :: LexerMonad ()
    }


data LexerState =
  LexerState {
      lexerStatePosition :: Position,
      lexerStatePreviousWasCarriageReturn :: Bool,
      lexerStatePreviousEndedLine :: Bool,
      lexerStateSavedPosition :: Maybe Position,
      lexerStateAccumulator :: Text,
      lexerStateData :: LexerStateData,
      lexerStateCurrentInput :: Maybe (Char, Classification)
    }


data Lexer =
  Lexer {
      lexerClassificationMap :: Map Char Classification,
      lexerMinorCategoryMap :: Map String Classification,
      lexerMajorCategoryMap :: Map String Classification,
      lexerQuoteMap :: Map Char [Char],
      lexerParenthesisMap :: Map Char [Char],
      lexerEscapeMap :: Map Char Char,
      lexerActionMap :: Map LexerStateData LexerStateDataActionMap
    }


newtype LexerMonad a =
  LexerMonad {
      lexerMonadAction :: StateT LexerState (Pipe Char Char Token () IO) a
    }


instance Monad LexerMonad where
  return value = LexerMonad $ return value
  a >>= b = LexerMonad $ do
    value <- lexerMonadAction a
    lexerMonadAction $ b value


defaultClassificationMap :: Map Char Classification
defaultClassificationMap =
  Map.fromList
    [-- Miscellaneous
     ('\x0009', HorizontalWhitespaceClassification),
     ('\x000A', VerticalWhitespaceClassification),
     ('\x000B', VerticalWhitespaceClassification),
     ('\x000C', VerticalWhitespaceClassification),
     ('\x000D', VerticalWhitespaceClassification),
     ('\x0022', QuoteClassification),
     ('\x0027', ApostropheClassification),
     ('\x002B', PlusClassification),
     ('\x002C', CommaClassification),
     ('\x002D', MinusClassification),
     ('\x002E', PeriodClassification),
     ('\x003A', ColonClassification),
     ('\x003B', SemicolonClassification),
     ('\x0040', AtSignClassification),
     ('\x005C', BackslashClassification),
     ('\x0060', BacktickClassification),
     -- Quotes
     ('\x00AB', QuoteClassification),
     ('\x00BB', QuoteClassification),
     ('\x2018', QuoteClassification),
     ('\x2019', QuoteClassification),
     ('\x201A', QuoteClassification),
     ('\x201B', QuoteClassification),
     ('\x201C', QuoteClassification),
     ('\x201D', QuoteClassification),
     ('\x201E', QuoteClassification),
     ('\x201F', QuoteClassification),
     ('\x2039', QuoteClassification),
     ('\x203A', QuoteClassification),
     ('\x2E02', QuoteClassification),
     ('\x2E03', QuoteClassification),
     ('\x2E04', QuoteClassification),
     ('\x2E05', QuoteClassification),
     ('\x2E09', QuoteClassification),
     ('\x2E0A', QuoteClassification),
     ('\x2E0C', QuoteClassification),
     ('\x2E0D', QuoteClassification),
     ('\x2E1C', QuoteClassification),
     ('\x2E1D', QuoteClassification),
     ('\x2E20', QuoteClassification),
     ('\x2E21', QuoteClassification),
     -- Parentheses
     ('\x0028', OpenParenthesisClassification),
     ('\x0029', CloseParenthesisClassification),
     ('\x005B', OpenParenthesisClassification),
     ('\x005D', CloseParenthesisClassification),
     ('\x007B', OpenParenthesisClassification),
     ('\x007D', CloseParenthesisClassification),
     ('\x2045', OpenParenthesisClassification),
     ('\x2046', CloseParenthesisClassification),
     ('\x239B', OpenParenthesisClassification),
     ('\x239C', OpenParenthesisClassification),
     ('\x239D', OpenParenthesisClassification),
     ('\x239E', CloseParenthesisClassification),
     ('\x239F', CloseParenthesisClassification),
     ('\x23A0', CloseParenthesisClassification),
     ('\x23A3', OpenParenthesisClassification),
     ('\x23A6', CloseParenthesisClassification),
     ('\x23A9', OpenParenthesisClassification),
     ('\x23A1', OpenParenthesisClassification),
     ('\x23A4', CloseParenthesisClassification),
     ('\x23A2', OpenParenthesisClassification),
     ('\x23A5', CloseParenthesisClassification),
     ('\x23A7', OpenParenthesisClassification),
     ('\x23A8', OpenParenthesisClassification),
     ('\x23AB', CloseParenthesisClassification),
     ('\x23AC', CloseParenthesisClassification),
     ('\x23AD', CloseParenthesisClassification),
     ('\x2774', OpenParenthesisClassification),
     ('\x2775', CloseParenthesisClassification),
     ('\x27E8', OpenParenthesisClassification),
     ('\x27E9', CloseParenthesisClassification),
     ('\x27EA', OpenParenthesisClassification),
     ('\x27EB', CloseParenthesisClassification),
     ('\x2768', OpenParenthesisClassification),
     ('\x2769', CloseParenthesisClassification),
     ('\x276A', OpenParenthesisClassification),
     ('\x276B', CloseParenthesisClassification),
     ('\x276C', OpenParenthesisClassification),
     ('\x276D', CloseParenthesisClassification),
     ('\x276E', OpenParenthesisClassification),
     ('\x276F', CloseParenthesisClassification),
     ('\x2770', OpenParenthesisClassification),
     ('\x2771', CloseParenthesisClassification),
     ('\x2772', OpenParenthesisClassification),
     ('\x2773', CloseParenthesisClassification),
     ('\x27E6', OpenParenthesisClassification),
     ('\x27E7', CloseParenthesisClassification),
     ('\x2983', OpenParenthesisClassification),
     ('\x2984', CloseParenthesisClassification),
     ('\x2985', OpenParenthesisClassification),
     ('\x2986', CloseParenthesisClassification),
     ('\x2987', OpenParenthesisClassification),
     ('\x2988', CloseParenthesisClassification),
     ('\x2989', OpenParenthesisClassification),
     ('\x298A', CloseParenthesisClassification),
     ('\x298B', OpenParenthesisClassification),
     ('\x298C', CloseParenthesisClassification),
     ('\x298D', OpenParenthesisClassification),
     ('\x298E', CloseParenthesisClassification),
     ('\x298F', OpenParenthesisClassification),
     ('\x2990', CloseParenthesisClassification),
     ('\x2991', OpenParenthesisClassification),
     ('\x2992', CloseParenthesisClassification),
     ('\x2997', OpenParenthesisClassification),
     ('\x2998', CloseParenthesisClassification),
     ('\x2E20', OpenParenthesisClassification),
     ('\x2E21', CloseParenthesisClassification),
     ('\x2E22', OpenParenthesisClassification),
     ('\x2E23', CloseParenthesisClassification),
     ('\x2E24', OpenParenthesisClassification),
     ('\x2E25', CloseParenthesisClassification),
     ('\x2E26', OpenParenthesisClassification),
     ('\x2E27', CloseParenthesisClassification),
     ('\x2E28', OpenParenthesisClassification),
     ('\x2E29', CloseParenthesisClassification),
     ('\x3008', OpenParenthesisClassification),
     ('\x3009', CloseParenthesisClassification),
     ('\x300A', OpenParenthesisClassification),
     ('\x300B', CloseParenthesisClassification),
     ('\x300C', OpenParenthesisClassification),
     ('\x300D', CloseParenthesisClassification),
     ('\x300E', OpenParenthesisClassification),
     ('\x300F', CloseParenthesisClassification),
     ('\x3010', OpenParenthesisClassification),
     ('\x3011', CloseParenthesisClassification),
     ('\x3014', OpenParenthesisClassification),
     ('\x3015', CloseParenthesisClassification),
     ('\x3016', OpenParenthesisClassification),
     ('\x3017', CloseParenthesisClassification),
     ('\x3018', OpenParenthesisClassification),
     ('\x3019', CloseParenthesisClassification),
     ('\xFF08', OpenParenthesisClassification),
     ('\xFF09', CloseParenthesisClassification),
     ('\xFF38', OpenParenthesisClassification),
     ('\xFF3D', CloseParenthesisClassification),
     ('\xFF5B', OpenParenthesisClassification),
     ('\xFF5D', CloseParenthesisClassification),
     ('\xFF5F', OpenParenthesisClassification),
     ('\xFF60', CloseParenthesisClassification)]


defaultMinorCategoryMap :: Map String Classification
defaultMinorCategoryMap =
  Map.fromList
    [("Zs", HorizontalWhitespaceClassification),
     ("Pc", ConnectingPunctuationClassification),
     ("Pd", ConnectingPunctuationClassification)]


defaultMajorCategoryMap :: Map String Classification
defaultMajorCategoryMap =
  Map.fromList
    [("L", LetterClassification),
     ("N", NumberClassification),
     ("S", OperatorClassification),
     ("M", OperatorClassification),
     ("P", OperatorClassification),
     ("Z", VerticalWhitespaceClassification)]


defaultQuoteMap :: Map Char [Char]
defaultQuoteMap =
  Map.fromList
    [('\x0022', ['\x0022']), -- Straight double
     ('\x00AB', ['\x00BB']), -- Double guillemet out-pointing
     ('\x00BB', ['\x00AB']), -- Double guillemet in-pointing
     ('\x2018', ['\x2019']), -- Single turned
     ('\x201A', ['\x2018', '\x2019']), -- Low-then-high, single
     ('\x201B', ['\x2018', '\x2019']), -- Single reversed
     ('\x201C', ['\x201D']), -- Double turned
     ('\x201D', ['\x201D']), -- Double right-only
     ('\x201E', ['\x201C', '\x201D']), -- Low-then-high, double
     ('\x201F', ['\x201D']), -- Double reversed
     ('\x2039', ['\x203A']), -- Single guillemet out-pointing
     ('\x203A', ['\x2039']), -- Single guillemet in-pointing
     ('\x2E02', ['\x2E03']), -- Substitution bracket
     ('\x2E04', ['\x2E05']), -- Dotted substitution bracket
     ('\x2E09', ['\x2E0A']), -- Transposition bracket
     ('\x2E0C', ['\x2E0D']), -- Raised omission bracket
     ('\x2E1C', ['\x2E1D']), -- Low paraphrase bracket
     ('\x2E20', ['\x2E21'])] -- Vertical bar with quill


defaultParenthesisMap :: Map Char [Char]
defaultParenthesisMap =
  Map.fromList
    [('\x0028', ['\x0029']), -- Parenthesis
     ('\x005B', ['\x005D']), -- Square bracket
     ('\x007B', ['\x007D']), -- Curly bracket
     ('\x2045', ['\x2046']), -- Square bracket with quill
     ('\x239B', ['\x239E']), -- Parenthesis upper hook
     ('\x239C', ['\x239F']), -- Parenthesis extension
     ('\x239D', ['\x23A0']), -- Parenthesis lower hook
     ('\x23A3', ['\x23A6']), -- Square bracket lower corner
     ('\x23A9', ['\x23AD']), -- Curly bracket lower hook
     ('\x23A1', ['\x23A4']), -- Square bracket upper corner
     ('\x23A2', ['\x23A5']), -- Square bracket extension
     ('\x23A7', ['\x23AB']), -- Curly bracket upper hook
     ('\x23A8', ['\x23AC']), -- Curly bracket middle piece
     ('\x2774', ['\x2775']), -- Medium curly bracket ornament
     ('\x27E8', ['\x27E9']), -- Mathematical angle bracket
     ('\x27EA', ['\x27EB']), -- Mathematical double angle bracket
     ('\x2768', ['\x2769']), -- Medium parenthesis ornament
     ('\x276A', ['\x276B']), -- Medium flattened parenthesis ornament
     ('\x276C', ['\x276D']), -- Medium angle bracket ornament
     ('\x276E', ['\x276F']), -- Heavy angle quotation mark ornament
     ('\x2770', ['\x2771']), -- Heavy angle bracket ornament
     ('\x2772', ['\x2773']), -- Light tortoise shell bracket ornament
     ('\x27E6', ['\x27E7']), -- Mathematical white square bracket
     ('\x2983', ['\x2984']), -- White curly bracket
     ('\x2985', ['\x2986']), -- White parenthesis
     ('\x2987', ['\x2988']), -- Z notation image bracket
     ('\x2989', ['\x298A']), -- Z notation binding bracket
     ('\x298B', ['\x298C']), -- Square bracket with underbar
     ('\x298D', ['\x298E', '\x2990']),
       -- Square bracket with tick, starting high
     ('\x298F', ['\x298E', '\x2990']),
       -- Square bracket with tick, starting low
     ('\x2991', ['\x2992']), -- Angle bracket with dot
     ('\x2997', ['\x2998']), -- Black tortoise shell bracket
     ('\x2E20', ['\x2E21']), -- Vertical bar with quill
     ('\x2E22', ['\x2E23', '\x2E25']), -- Half bracket, starting high
     ('\x2E24', ['\x2E23', '\x2E25']), -- Half bracket, starting low
     ('\x2E26', ['\x2E27']), -- Sideways U bracket
     ('\x2E28', ['\x2E29']), -- Double parenthesis
     ('\x3008', ['\x3009']), -- Angle bracket
     ('\x300A', ['\x300B']), -- Double angle bracket
     ('\x300C', ['\x300D']), -- Corner bracket
     ('\x300E', ['\x300F']), -- White corner bracket
     ('\x3010', ['\x3011']), -- Black lenticular bracket
     ('\x3014', ['\x3015']), -- Tortoise shell bracket
     ('\x3016', ['\x3017']), -- White lenticular bracket
     ('\x3018', ['\x3019']), -- White tortoise shell bracket
     ('\xFF08', ['\xFF09']), -- Fullwidth parenthesis
     ('\xFF38', ['\xFF3D']), -- Fullwidth square bracket
     ('\xFF5B', ['\xFF5D']), -- Fullwidth curly bracket
     ('\xFF5F', ['\xFF60'])] -- Fullwidth white parenthesis


defaultEscapeMap :: Map Char Char
defaultEscapeMap =
  Map.fromList
    [('\x005C', '\x005C'), -- Backslash
     ('\x006E', '\x000A'), -- Newline
     ('\x0072', '\x000D'), -- Carriage return
     -- Quotes
     ('\x0022', '\x0022'),
     ('\x00AB', '\x00AB'),
     ('\x00BB', '\x00BB'),
     ('\x2018', '\x2018'),
     ('\x2019', '\x2019'),
     ('\x201A', '\x201A'),
     ('\x201B', '\x201B'),
     ('\x201C', '\x201C'),
     ('\x201D', '\x201D'),
     ('\x201E', '\x201E'),
     ('\x201F', '\x201F'),
     ('\x2039', '\x2039'),
     ('\x203A', '\x203A'),
     ('\x2E02', '\x2E02'),
     ('\x2E03', '\x2E03'),
     ('\x2E04', '\x2E04'),
     ('\x2E05', '\x2E05'),
     ('\x2E09', '\x2E09'),
     ('\x2E0A', '\x2E0A'),
     ('\x2E0C', '\x2E0C'),
     ('\x2E0D', '\x2E0D'),
     ('\x2E1C', '\x2E1C'),
     ('\x2E1D', '\x2E1D'),
     ('\x2E20', '\x2E20'),
     ('\x2E21', '\x2E21')]


defaultActionMap :: Map LexerStateData LexerStateDataActionMap
defaultActionMap =
  Map.fromList
    [(TopLevelLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(LetterClassification, actionStartWord),
               (NumberClassification, actionStartNumber),
               (OperatorClassification, actionStartOperator),
               (AtSignClassification, actionStartOperator),
               (HorizontalWhitespaceClassification, actionStartWhitespace),
               (VerticalWhitespaceClassification, actionStartWhitespace),
               (PlusClassification, actionStartPlus),
               (MinusClassification, actionStartMinus),
               (ConnectingPunctuationClassification, actionStartDash),
               (OpenParenthesisClassification, actionStartOpenParenthesis),
               (CloseParenthesisClassification, actionStartCloseParenthesis),
               (QuoteClassification, actionStartString),
               (PeriodClassification, actionStartPeriod),
               (CommaClassification, actionStartSplice),
               (ApostropheClassification, actionStartTick),
               (BacktickClassification, actionStartBacktick)],
          lexerStateDataActionMapEndAction = Just actionEnd,
          lexerStateDataActionMapDefaultAction = actionError
        }),
     (WordLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(LetterClassification, actionContinue),
               (ApostropheClassification, actionContinue),
               (MinusClassification, actionWordToHyphen),
               (ConnectingPunctuationClassification, actionWordToHyphen),
               (CommaClassification, actionWordToComma),
               (ColonClassification, actionWordToColon),
               (SemicolonClassification, actionWordToSemicolon)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (NumberLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(PeriodClassification, actionContinue)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (OperatorLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(OperatorClassification, actionContinue),
               (PlusClassification, actionContinue),
               (MinusClassification, actionContinue),
               (AtSignClassification, actionContinue)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (PeriodLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(PeriodClassification, actionContinue),
               (NumberClassification, actionPeriodToNumber)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishPeriod
        }),
     (PlusLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(NumberClassification, actionPlusToNumber),
               (PlusClassification, actionPlusToOperator),
               (MinusClassification, actionPlusToOperator),
               (OperatorClassification, actionPlusToOperator),
               (AtSignClassification, actionPlusToOperator)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (MinusLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(NumberClassification, actionMinusToNumber),
               (PlusClassification, actionMinusToOperator),
               (MinusClassification, actionContinue),
               (OperatorClassification, actionMinusToOperator),
               (AtSignClassification, actionMinusToOperator),
               (ConnectingPunctuationClassification, actionMinusToDash)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishMinus
        }),
     (HyphenLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(MinusClassification, actionContinue),
               (ConnectingPunctuationClassification, actionContinue)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (DashLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(MinusClassification, actionContinue),
               (ConnectingPunctuationClassification, actionContinue)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (StringLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(QuoteClassification, actionMaybeFinishString),
               (BackslashClassification, actionPushEscapeSequence)],
          lexerStateDataActionMapEndAction = Just actionUnexpectedEndInString,
          lexerStateDataActionMapDefaultAction =
            actionContinueAndAccumulateCharacter
        }),
     (EscapeSequenceLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionHandleEscapeSequence
        }),
     (OpenParenthesisLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (CloseParenthesisLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(MinusClassification, actionParenthesisToHyphen)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (PreCommaLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(CommaClassification, actionContinueComma)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionError
        }),
     (CommaLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (PreColonLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(ColonClassification, actionContinueColon)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionError
        }),
     (ColonLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (PreSemicolonLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(SemicolonClassification, actionContinueSemicolon)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionError
        }),
     (SemicolonLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (SpliceLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(AtSignClassification, actionStartListSplice)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (ListSpliceLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinish
        }),
     (WhitespaceLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(HorizontalWhitespaceClassification, actionContinue),
               (VerticalWhitespaceClassification, actionContinue)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishWhitespace
        })]


defaultLexer :: Lexer
defaultLexer =
  Lexer {
      lexerClassificationMap = defaultClassificationMap,
      lexerMinorCategoryMap = defaultMinorCategoryMap,
      lexerMajorCategoryMap = defaultMajorCategoryMap,
      lexerQuoteMap = defaultQuoteMap,
      lexerParenthesisMap = defaultParenthesisMap,
      lexerEscapeMap = defaultEscapeMap,
      lexerActionMap = defaultActionMap
    }


classify :: Lexer -> Char -> Classification
classify lexer character =
  let classificationMap = lexerClassificationMap lexer
      minorCategoryMap = lexerMinorCategoryMap lexer
      majorCategoryMap = lexerMajorCategoryMap lexer
      minorCategory = fromMaybe "??" $ categorize character
      majorCategory = take 1 minorCategory
      maybeDirect = Map.lookup character classificationMap
      maybeMinorCategoryBased = Map.lookup minorCategory minorCategoryMap
      maybeMajorCategoryBased = Map.lookup majorCategory majorCategoryMap
  in case maybeDirect of
       Just result -> result
       Nothing -> case maybeMinorCategoryBased of
                    Just result -> result
                    Nothing -> case maybeMajorCategoryBased of
                                 Just result -> result
                                 Nothing -> UnknownClassification


runLexer :: (MonadIO m) => Lexer -> Conduit ByteString m Token
runLexer lexer = do
  let loopTexts :: (Monad m) => Conduit Text m Char
      loopTexts = C.mapM_ $ \text -> mapM_ yield $ T.unpack text
      process :: (MonadIO m) => Conduit Char m Token
      process = do
        let initialPosition = Position {
                                  positionOffset = 0,
                                  positionByte = 0,
                                  positionLine = 1,
                                  positionColumn = 1
                                }
            state = LexerState {
                        lexerStatePosition = initialPosition,
                        lexerStatePreviousWasCarriageReturn = False,
                        lexerStatePreviousEndedLine = False,
                        lexerStateSavedPosition = Nothing,
                        lexerStateAccumulator = T.empty,
                        lexerStateData = TopLevelLexerStateData,
                        lexerStateCurrentInput = Nothing
                      }
        _ <- runStateT state $ do
          loopCharacters
          -- TODO fire the appropriate end-of-input action
        return ()
      loopCharacters :: LexerMonad ()
      loopCharacters = do
        maybeCharacter <- await
        case maybeCharacter of
          Nothing -> return ()
          Just character -> do
            -- todo instead of this, fire the appropriate action
            startToken
            mapM_ (\_ -> consumeCharacter) [0 .. 79]
            endToken TokenType
            loopCharacters
  C.decode C.utf8 $= loopTexts =$= process


startPosition :: Position
startPosition =
  Position {
      positionOffset = 0,
      positionByte = 0,
      positionLine = 1,
      positionColumn = 1
    }


initialLexerState :: LexerState
initialLexerState =
  LexerState {
      lexerStatePosition = startPosition,
      lexerStatePreviousWasCarriageReturn = False,
      lexerStateSavedPosition = Nothing,
      lexerStateData = TopLevelLexerStateData,
      lexerStateCurrentInput = Nothing
    }


lexerAction :: Lexer -> LexerState -> Maybe Char -> LexerMonad ()
lexerAction lexer state maybeCharacter =
  case Map.lookup (lexerStateData state) (lexerActionMap lexer) of
    Nothing -> actionError
    Just actionMap ->
      case maybeCharacter of
        Nothing ->
          case lexerStateDataActionMapEndAction actionMap of
            Nothing -> lexerStateDataActionMapDefaultAction actionMap
            Just action -> action
        Just character ->
          case Map.lookup (classify lexer character)
                          (lexerStateDataActionMapClassificationActionMap
                            actionMap) of
            Nothing -> lexerStateDataActionMapDefaultAction actionMap
            Just action -> action


startToken :: LexerMonad ()
startToken = LexerMonad $ do
  state <- get
  let position = lexerStatePosition state
  put $ state {
            lexerStateSavedPosition = Just position
          }


endToken :: TokenType -> LexerMonad ()
endToken tokenType = LexerMonad $ do
  state <- get
  case lexerStateSavedPosition state of
    Nothing -> return ()
    Just startPosition -> do
      let endPosition = lexerStatePosition state
          text = lexerStateAccumulator state
      put $ state {
                lexerStateSavedPosition = Nothing,
                lexerStateAccumulator = Text.empty
              }
      lift $ yield $ Token {
                         tokenType = tokenType,
                         tokenSpan = Span {
                                         spanStart = startPosition,
                                         spanEnd = startPosition
                                       },
                         tokenText = text
                       }


produceError :: Error -> LexerMonad ()
produceError error = undefined


produceToken :: Token -> LexerMonad ()
produceToken token = undefined


consumeCharacter :: LexerMonad ()
consumeCharacter = LexerMonad $ do
  oldState <- get
  newCharacter <- await
  let oldCharacter = lexerStateCharacter oldState
      oldPosition = lexerStatePosition state
      byteLength = BS.length $ T.encodeUtf8 $ T.singleton previousCharacter
      previousWasCarriageReturn = lexerStatePreviousWasCarriageReturn oldState
      previousEndedLine = lexerStatePreviousEndedLine oldState
      (isCarriageReturn, endsLine, startsNewLine) =
        case (previousWasCarriageReturn, previousEndedLine,  of
      newLine = if startsNewLine
                  then positionLine oldPosition + 1
                  else positionLine oldPosition
      newColumn = if startsNewLine
                    then 1
                    else positionColumn oldPosition + 1
      newPosition = oldPosition {
                        positionCharacter = positionCharacter oldPosition + 1,
                        positionByte = positionByte oldPosition + byteLength,
                        positionLine = newLine,
                        positionColumn newColumn
                      }
      newAccumulator = T.snoc (lexerStateAccumulator oldState) oldCharacter
      newInput = Just (newCharacter, newClassification)
      newState = oldState {
                     lexerStatePosition newPosition,
                     lexerStatePreviousWasCarriageReturn :: Bool,
                     lexerStatePreviousEndedLine :: Bool,
                     lexerStateAccumulator = newAccumulator,
                     lexerStateCurrentInput = newInput
                   }
        this._byte += Unicode.codepointByteCount(this._input[this._offset]);
        
        this._offset++;
        
        if(character == '\n') {
            if(!this._previousWasCarriageReturn) {
                this._line++;
                this._column = 1;
            }
            this._previousWasCarriageReturn = false;
        } else if(character == '\r') {
            this._line++;
            this._column = 1;
            this._previousWasCarriageReturn = true;
        } else if(classification == VerticalWhitespaceClassification) {
            this._line++;
            this._column = 1;
            this._previousWasCarriageReturn = false;
        } else {
            this._column++;
            this._previousWasCarriageReturn = false;
        }


pushStateData :: LexerStateData -> LexerMonad ()
pushStateData stateData = undefined


dropStateData :: LexerMonad ()
dropStateData = undefined


actionError :: LexerMonad ()
actionError = undefined
{-
            initResult();
            consumeCharacter();
            fillResult();
            
            result.type = 'error';
            result.character = character;
            result.message =
              'Unexpected character ' + Unicode.showCharacter(character) + '.';
            
            return result;
-}


actionEnd :: LexerMonad ()
actionEnd = undefined
{-
            -- TODO
-}


actionContinue :: LexerMonad ()
actionContinue = undefined
{-
            consumeCharacter();
-}


actionFinish :: LexerMonad ()
actionFinish = undefined
{-
            fillResult();
            result.type = state;
            
            this._stateStack.pop();
            return result;
-}


actionStartWord :: LexerMonad ()
actionStartWord = undefined
{-
            this._stateStack.push('word');
            consumeCharacter();
-}


actionWordToHyphen :: LexerMonad ()
actionWordToHyphen = undefined
{-
            fillResult();
            result.type = state;
            
            this._stateStack.pop();
            this._stateStack.push('hyphen');
            return result;
-}


actionWordToComma :: LexerMonad ()
actionWordToComma = undefined
{-
            fillResult();
            result.type = state;
            
            this._stateStack.pop();
            this._stateStack.push('pre-comma');
            return result;
-}


actionWordToColon :: LexerMonad ()
actionWordToColon = undefined
{-
            fillResult();
            result.type = state;
            
            this._stateStack.pop();
            this._stateStack.push('pre-colon');
            return result;
-}


actionWordToSemicolon :: LexerMonad ()
actionWordToSemicolon = undefined
{-
            fillResult();
            result.type = state;
            
            this._stateStack.pop();
            this._stateStack.push('pre-semicolon');
            return result;
-}


actionStartNumber :: LexerMonad ()
actionStartNumber = undefined
{-
            this._stateStack.push('number');
            consumeCharacter();
-}


actionStartOperator :: LexerMonad ()
actionStartOperator = undefined
{-
            this._stateStack.push('operator');
            consumeCharacter();
-}


actionStartSplice :: LexerMonad ()
actionStartSplice = undefined
{-
            this._stateStack.push('splice');
            consumeCharacter();
-}


actionStartListSplice :: LexerMonad ()
actionStartListSplice = undefined
{-
            this._stateStack.pop();
            this._stateStack.push('list-splice');
            consumeCharacter();
-}


actionStartTick :: LexerMonad ()
actionStartTick = undefined
{-
            consumeCharacter();
            fillResult();
            result.type = 'tick';
            
            return result;
-}


actionStartBacktick :: LexerMonad ()
actionStartBacktick = undefined
{-
            consumeCharacter();
            fillResult();
            result.type = BacktickClassification;
            
            return result;
-}


actionContinueComma :: LexerMonad ()
actionContinueComma = undefined
{-
            this._stateStack.pop();
            this._stateStack.push(CommaClassification);
            consumeCharacter();
-}


actionContinueColon :: LexerMonad ()
actionContinueColon = undefined
{-
            this._stateStack.pop();
            this._stateStack.push(ColonClassification);
            consumeCharacter();
-}


actionContinueSemicolon :: LexerMonad ()
actionContinueSemicolon = undefined
{-
            this._stateStack.pop();
            this._stateStack.push(SemicolonClassification);
            consumeCharacter();
-}


actionStartPlus :: LexerMonad ()
actionStartPlus = undefined
{-
            this._stateStack.push(PlusClassification);
            consumeCharacter();
-}


actionPlusToNumber :: LexerMonad ()
actionPlusToNumber = undefined
{-
            this._stateStack.pop();
            this._stateStack.push('number');
            consumeCharacter();
-}


actionPlusToOperator :: LexerMonad ()
actionPlusToOperator = undefined
{-
            this._stateStack.pop();
            this._stateStack.push('operator');
            consumeCharacter();
-}


actionStartMinus :: LexerMonad ()
actionStartMinus = undefined
{-
            this._stateStack.push(MinusClassification);
            consumeCharacter();
-}


actionMinusToNumber :: LexerMonad ()
actionMinusToNumber = undefined
{-
            this._stateStack.pop();
            this._stateStack.push('number');
            consumeCharacter();
-}


actionMinusToOperator :: LexerMonad ()
actionMinusToOperator = undefined
{-
            this._stateStack.pop();
            this._stateStack.push('operator');
            consumeCharacter();
-}


actionMinusToDash :: LexerMonad ()
actionMinusToDash = undefined
{-
            -- TODO
-}


actionFinishMinus :: LexerMonad ()
actionFinishMinus = undefined
{-
            fillResult();
            result.type = 'operator';
            
            this._stateStack.pop();
            return result;
-}


actionStartDash :: LexerMonad ()
actionStartDash = undefined
{-
            this._stateStack.push('dash');
            consumeCharacter();
-}


actionStartString :: LexerMonad ()
actionStartString = undefined
{-
            result.value = '';
            
            this._stateStack.push('string');
            consumeCharacter();
-}


actionContinueAndAccumulateCharacter :: LexerMonad ()
actionContinueAndAccumulateCharacter = undefined
{-
            result.value += character;
            consumeCharacter();
-}


actionUnexpectedEndInString :: LexerMonad ()
actionUnexpectedEndInString = undefined
{-
            var tempResult = result;
            initResult();
            fillResult();
            this._stateStack.pop();
            this._savedResult = tempResult;
            
            result.type = 'error';
            result.message = 'Unexpected end of input in string.';
            
            return result;
-}


actionMaybeFinishString :: LexerMonad ()
actionMaybeFinishString = undefined
{-
            var startQuote = Unicode.codepointToKey
                  (this._input[result.position.offset]);
            var endQuote = Unicode.codepointToKey(codepoint);
            
            if(_.any(this.quoteMap[startQuote], function(item) {
                return item == endQuote;
            })) {
                consumeCharacter();
                fillResult();
                result.type = 'string';
                
                this._stateStack.pop();
                return result;
            } else {
                result.value += character;
                consumeCharacter();
            }
-}


actionPushEscapeSequence :: LexerMonad ()
actionPushEscapeSequence = undefined
{-
            this._stateStack.push('escape-sequence');
            escapeSequence = { start: this._offset };
            consumeCharacter();
-}


actionHandleEscapeSequence :: LexerMonad ()
actionHandleEscapeSequence = undefined
{-
            if(this._offset == escapeSequence.start + 1) {
                escapeSequence.type = Unicode.codepointToKey(codepoint);
                escapeSequence.value = this.escapeMap[escapeSequence.type];
                if(escapeSequence.value) {
                    result.value +=
                      (new Unicode.CodePoint(parseInt
                        (escapeSequence.value, 16))).toString();
                    consumeCharacter();
                    this._stateStack.pop();
                } else if(escapeSequence.type == '0075') {
                    escapeSequence.length = 4;
                    consumeCharacter();
                } else if(escapeSequence.type == '0055') {
                    escapeSequence.length = 8;
                    consumeCharacter();
                } else {
                    var tempResult = result;
                    initResult();
                    consumeCharacter();
                    fillResult();
                    this._stateStack.pop();
                    this._savedResult = tempResult;
                    
                    result.type = 'error';
                    result.message =
                      'Unexpected escape sequence type '
                      + Unicode.showCharacter(character)
                      + ' in string.';
                    
                    return result;
                }
            } else if(this._offset == escapeSequence.start + 2
                      + escapeSequence.length)
            {
                escapeSequence.value =
                  Unicode.getString
                    (this._input.slice(escapeSequence.start + 2,
                                       escapeSequence.start + 2
                                       + escapeSequence.length));
                try {
                    result.value +=
                      (new Unicode.CodePoint
                        (parseInt(escapeSequence.value, 16))).toString();
                } catch(error) {
                    var tempResult = result;
                    initResult();
                    fillResult();
                    this._stateStack.pop();
                    this._savedResult = tempResult;
                    
                    result.type = 'error';
                    result.message =
                      'Invalid Unicode hex escape '
                      + Unicode.showString(escapeSequence.value)
                      + ' in string.';
                    
                    return result;
                }
                
                this._stateStack.pop();
            } else {
                consumeCharacter();
            }
-}


actionStartOpenParenthesis :: LexerMonad ()
actionStartOpenParenthesis = undefined
{-
            this._stateStack.push(OpenParenthesisClassification);
            consumeCharacter();
-}


actionStartCloseParenthesis :: LexerMonad ()
actionStartCloseParenthesis= undefined
{-
            this._stateStack.push(CloseParenthesisClassification);
            consumeCharacter();
-}


actionParenthesisToHyphen :: LexerMonad ()
actionParenthesisToHyphen = undefined
{-
            fillResult();
            result.type = state;
            
            this._stateStack.pop();
            this._stateStack.push('hyphen');
            return result;
-}


actionStartPeriod :: LexerMonad ()
actionStartPeriod = undefined
{-
            this._stateStack.push(PeriodClassification);
            consumeCharacter();
-}


actionPeriodToNumber :: LexerMonad ()
actionPeriodToNumber = undefined
{-
            -- TODO
-}


actionFinishPeriod :: LexerMonad ()
actionFinishPeriod = undefined
{-
            fillResult();
            
            if(result.string == '.') result.type = PeriodClassification;
            else if(result.string == '..') result.type = 'operator';
            else if(result.string == '...') result.type = 'ellipsis';
            else {
                result.type = 'error';
                result.message = 'Too many consecutive periods.';
            }
            
            this._stateStack.pop();
            return result;
-}


actionStartWhitespace :: LexerMonad ()
actionStartWhitespace = undefined
{-
            this._stateStack.push('whitespace');
            consumeCharacter();
-}


actionFinishWhitespace :: LexerMonad ()
actionFinishWhitespace = undefined
{-
            fillResult();
            
            var codepoints =
              this._input.slice
                (result.position.offset,
                 result.position.offset + result.position.length);
            
            var newlineCount = 0;
            var previousWasCarriageReturn = false;
            for(var i = 0; i < codepoints.length; i++) {
                var codepoint = codepoints[i];
                var character = codepoint.toString();
                var classification = this.classifyCodepoint(codepoint);
                
                if(character == '\n') {
                    if(!previousWasCarriageReturn) newlineCount++;
                    previousWasCarriageReturn = false;
                } else if(character == '\r') {
                    newlineCount++;
                    previousWasCarriageReturn = true;
                } else if(classification == VerticalWhitespaceClassification) {
                    newlineCount++;
                    previousWasCarriageReturn = false;
                } else {
                    previousWasCarriageReturn = false;
                }
            }
            
            if(newlineCount > 1) result.type = 'paragraph-break';
            else result.type = 'space';
            
            this._stateStack.pop();
            return result;
-}


{-

lex: function() {
    var result, escapeSequence, codepoint, character, classification;
    
    var initResult = _.bind(function() {
        if(this._savedResult) {
            result = this._savedResult;
            this._savedResult = null;
        } else {
            result = {
            position: {
            offset: this._offset,
            byte: this._byte,
            line: this._line,
            column: this._column,
            },
            };
        }
    }, this);
    
    var fillResult = _.bind(function() {
        result.position.length = this._offset - result.position.offset;
        result.string =
          Unicode.getString(this._input.slice
            (result.position.offset, this._offset));
    }, this);
    
    initResult();
    
    if(this._offset >= this._input.length) {
        fillResult();
        result.type = 'end';
        return result;
    }
    
    while(true) {
        if(this._offset < this._input.length) {
            codepoint = this._input[this._offset];
            character = codepoint.toString();
            classification = this.classifyCodepoint(codepoint);
        } else {
            codepoint = null;
            character = null;
            classification = 'end';
        }
        
        var state = _.last(this._stateStack) || TopLevelLexerStateData;
        
        var action;
        -- Decode the action map
        
        
        /*
        console.log('[' + _.reduce(this._stateStack, function(soFar, item) {
            if(soFar != '') soFar += ' ';
            return soFar + item;
        }, '') + '] '
        + (character ? Unicode.showCharacter(character) : '(null)')
        + ' ' + action);
        */
    }
},
});


module.exports = Lexer;
-}
