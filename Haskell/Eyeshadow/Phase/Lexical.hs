{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Eyeshadow.Phase.Lexical
  (lex)
  where

import qualified Data.ByteString as ByteString
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Char
import Data.Maybe

import Eyeshadow.Data.Unicode
import Eyeshadow.Phase.Lexical.Types
import Eyeshadow.Phase.Lexical.Actions
import Eyeshadow.Prelude


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
          lexerStateDataActionMapDefaultAction = actionFinishWord
        }),
     (NumberLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(PeriodClassification, actionContinue)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishNumber
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
          lexerStateDataActionMapDefaultAction = actionFinishOperator
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
          lexerStateDataActionMapDefaultAction = actionFinishPlus
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
          lexerStateDataActionMapDefaultAction = actionFinishHyphen
        }),
     (DashLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(MinusClassification, actionContinue),
               (ConnectingPunctuationClassification, actionContinue)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishDash
        }),
     (StringLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(QuoteClassification, actionMaybeFinishString),
               (BackslashClassification, actionEscapeSequence)],
          lexerStateDataActionMapEndAction = Just actionUnexpectedEndInString,
          lexerStateDataActionMapDefaultAction = actionContinueString
        }),
     (OpenParenthesisLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishOpenParenthesis
        }),
     (CloseParenthesisLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(MinusClassification, actionParenthesisToHyphen)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishCloseParenthesis
        }),
     (CommaLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishComma
        }),
     (ColonLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishColon
        }),
     (SemicolonLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishSemicolon
        }),
     (SpliceLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap =
            Map.fromList
              [(AtSignClassification, actionStartListSplice)],
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishSplice
        }),
     (ListSpliceLexerStateData,
      LexerStateDataActionMap {
          lexerStateDataActionMapClassificationActionMap = Map.empty,
          lexerStateDataActionMapEndAction = Nothing,
          lexerStateDataActionMapDefaultAction = actionFinishListSplice
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


lex
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Conduit.Conduit (Char, Span) (Eff.Eff r) Token
lex = do
  let process :: (Eff.Member Diagnose r,
                  Eff.Member (Eff.Reader Lexer) r,
                  Eff.Member (Eff.State LexerState) r)
              => Conduit Char (Eff.Eff r) Token
      process = do
        _ <- flip runStateT (defaultLexer, initialLexerState) $ lexerMonadAction $ do
            consumeCharacter
            loopCharacters
        return ()
      loopCharacters :: LexerMonad ()
      loopCharacters = do
        (_, state) <- LexerMonad $ get
        if lexerStateDone state
          then return ()
          else do
            lexerAction
            loopCharacters
  Eff.runReader defaultLexer $ Eff.runState initialLexerState $ ...


stripSpaces :: (Monad m) => Conduit Token m Token
stripSpaces = do
  maybeToken <- await
  case maybeToken of
    Nothing -> return ()
    Just token -> do
      if tokenType token == SpaceTokenType
        then return ()
        else yield token
      stripSpaces


initialPosition :: Position
initialPosition =
  Position {
      positionCharacter = 1,
      positionByte = 0,
      positionLine = 1,
      positionColumn = 1
    }


initialLexerState :: LexerState
initialLexerState =
  LexerState {
      lexerStatePosition = initialPosition,
      lexerStateSavedPosition = Nothing,
      lexerStateAccumulator = T.empty,
      lexerStateValue = Nothing,
      lexerStateOpenDelimiter = Nothing,
      lexerStateCloseDelimiter = Nothing,
      lexerStateData = TopLevelLexerStateData,
      lexerStateInput = Nothing,
      lexerStateDone = False
    }


lexerAction :: LexerMonad ()
lexerAction = do
  (lexer, state) <- LexerMonad $ get
  case Map.lookup (lexerStateData state) (lexerActionMap lexer) of
    Nothing -> done
    Just actionMap ->
      case lexerStateInput state of
        Nothing ->
          case lexerStateDataActionMapEndAction actionMap of
            Nothing -> lexerStateDataActionMapDefaultAction actionMap
            Just action -> action
        Just (character, classification) ->
          case Map.lookup classification
                          (lexerStateDataActionMapClassificationActionMap
                            actionMap) of
            Nothing -> lexerStateDataActionMapDefaultAction actionMap
            Just action -> action
