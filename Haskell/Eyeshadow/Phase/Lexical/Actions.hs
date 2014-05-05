{-# LANGUAGE NoImplicitPrelude #-}
module Eyeshadow.Phase.Lexical.Actions
  (actionError,
   actionEnd,
   actionContinue,
   actionStartWord,
   actionWordToHyphen,
   actionWordToComma,
   actionWordToColon,
   actionWordToSemicolon,
   actionFinishWord,
   actionFinishHyphen,
   actionFinishComma,
   actionFinishColon,
   actionFinishSemicolon,
   actionStartNumber,
   actionFinishNumber,
   actionStartOperator,
   actionFinishOperator,
   actionStartSplice,
   actionFinishSplice,
   actionStartListSplice,
   actionFinishListSplice,
   actionStartTick,
   actionStartBacktick,
   actionStartPlus,
   actionPlusToNumber,
   actionPlusToOperator,
   actionFinishPlus,
   actionStartMinus,
   actionMinusToNumber,
   actionMinusToOperator,
   actionMinusToDash,
   actionFinishMinus,
   actionStartDash,
   actionFinishDash,
   actionStartString,
   actionContinueString,
   actionUnexpectedEndInString,
   actionMaybeFinishString,
   actionEscapeSequence,
   actionHelperAppendInputToValue,
   actionHelperAppendCharacterToValue,
   actionStartOpenParenthesis,
   actionFinishOpenParenthesis,
   actionStartCloseParenthesis,
   actionFinishCloseParenthesis,
   actionParenthesisToHyphen,
   actionStartPeriod,
   actionPeriodToNumber,
   actionFinishPeriod,
   actionStartWhitespace,
   actionFinishWhitespace)
  where

import Data.Maybe

import Eyeshadow.Data.Token
import Eyeshadow.Phase.Lexical.Primitives
import Eyeshadow.Phase.Lexical.Types
import Eyeshadow.Prelude



actionError :: Action ()
actionError = error "actionError not implemented"
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


actionEnd :: Action ()
actionEnd = do
  done


actionContinue :: Action ()
actionContinue = do
  consumeCharacter


actionStartWord :: Action ()
actionStartWord = do
  startToken
  consumeCharacter
  setStateData WordLexerStateData


actionWordToHyphen :: Action ()
actionWordToHyphen = do
  endToken WordTokenType
  startToken
  consumeCharacter
  setStateData HyphenLexerStateData


actionWordToComma :: Action ()
actionWordToComma = do
  endToken WordTokenType
  startToken
  consumeCharacter
  setStateData CommaLexerStateData


actionWordToColon :: Action ()
actionWordToColon = do
  endToken WordTokenType
  startToken
  consumeCharacter
  setStateData ColonLexerStateData


actionWordToSemicolon :: Action ()
actionWordToSemicolon = do
  endToken WordTokenType
  startToken
  consumeCharacter
  setStateData SemicolonLexerStateData


actionFinishWord :: Action ()
actionFinishWord = do
  endToken WordTokenType
  setStateData TopLevelLexerStateData


actionFinishHyphen :: Action ()
actionFinishHyphen = do
  endToken HyphenTokenType
  setStateData TopLevelLexerStateData


actionFinishComma :: Action ()
actionFinishComma = do
  endToken CommaTokenType
  setStateData TopLevelLexerStateData


actionFinishColon :: Action ()
actionFinishColon = do
  endToken ColonTokenType
  setStateData TopLevelLexerStateData


actionFinishSemicolon :: Action ()
actionFinishSemicolon = do
  endToken SemicolonTokenType
  setStateData TopLevelLexerStateData


actionStartNumber :: Action ()
actionStartNumber = do
  setStateData NumberLexerStateData
  consumeCharacter


actionFinishNumber :: Action ()
actionFinishNumber = do
  text <- getAccumulator
  setValue $ Just text
  endToken NumberTokenType
  setStateData TopLevelLexerStateData


actionStartOperator :: Action ()
actionStartOperator = do
  startToken
  consumeCharacter
  setStateData OperatorLexerStateData


actionFinishOperator :: Action ()
actionFinishOperator = do
  endToken OperatorTokenType
  setStateData TopLevelLexerStateData


actionStartSplice :: Action ()
actionStartSplice = do
  startToken
  consumeCharacter
  setStateData SpliceLexerStateData


actionFinishSplice :: Action ()
actionFinishSplice = do
  endToken SpliceTokenType
  setStateData TopLevelLexerStateData


actionStartListSplice :: Action ()
actionStartListSplice = do
  consumeCharacter
  setStateData ListSpliceLexerStateData


actionFinishListSplice :: Action ()
actionFinishListSplice = do
  endToken ListSpliceTokenType
  setStateData TopLevelLexerStateData


actionStartTick :: Action ()
actionStartTick = do
  startToken
  consumeCharacter
  endToken TickTokenType


actionStartBacktick :: Action ()
actionStartBacktick = do
  startToken
  consumeCharacter
  endToken BacktickTokenType


actionStartPlus :: Action ()
actionStartPlus = do
  startToken
  consumeCharacter
  setStateData PlusLexerStateData


actionPlusToNumber :: Action ()
actionPlusToNumber = do
  consumeCharacter
  setStateData NumberLexerStateData


actionPlusToOperator :: Action ()
actionPlusToOperator = do
  consumeCharacter
  setStateData OperatorLexerStateData


actionFinishPlus :: Action ()
actionFinishPlus = do
  endToken OperatorTokenType
  setStateData TopLevelLexerStateData


actionStartMinus :: Action ()
actionStartMinus = do
  startToken
  consumeCharacter
  setStateData MinusLexerStateData


actionMinusToNumber :: Action ()
actionMinusToNumber = do
  consumeCharacter
  setStateData NumberLexerStateData


actionMinusToOperator :: Action ()
actionMinusToOperator = do
  consumeCharacter
  setStateData OperatorLexerStateData


actionMinusToDash :: Action ()
actionMinusToDash = do
  consumeCharacter
  setStateData DashLexerStateData


actionFinishMinus :: Action ()
actionFinishMinus = do
  endToken OperatorTokenType
  setStateData TopLevelLexerStateData


actionStartDash :: Action ()
actionStartDash = do
  startToken
  consumeCharacter
  setStateData DashLexerStateData


actionFinishDash :: Action ()
actionFinishDash = do
  endToken DashTokenType
  setStateData TopLevelLexerStateData


actionStartString :: Action ()
actionStartString = do
  setValue $ Just T.empty
  setStateData StringLexerStateData
  maybeInput <- getInput
  case maybeInput of
    Nothing -> setOpenDelimiter Nothing
    Just (character, _) -> setOpenDelimiter $ Just character
  consumeCharacter


actionContinueString :: Action ()
actionContinueString = do
  actionHelperAppendInputToValue
  consumeCharacter


actionUnexpectedEndInString :: Action ()
actionUnexpectedEndInString = error "actionUnexpectedEndInString not implemented"
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


actionMaybeFinishString :: Action ()
actionMaybeFinishString = do
  openDelimiter <- getOpenDelimiter
  maybeInput <- getInput
  lexer <- getLexer
  let closeDelimiter = case maybeInput of
                         Nothing -> Nothing
                         Just (character, _) -> Just character
      startQuote = fromMaybe '"' openDelimiter
      endQuote = fromMaybe '"' closeDelimiter
      matches = case Map.lookup startQuote (lexerQuoteMap lexer) of
                  Nothing -> False
                  Just allowed -> elem endQuote allowed
  if matches
    then do
      setCloseDelimiter closeDelimiter
      consumeCharacter
      endToken StringTokenType
      setStateData TopLevelLexerStateData
    else do
      actionHelperAppendInputToValue
      consumeCharacter


actionEscapeSequence :: Action ()
actionEscapeSequence = do
  lexer <- getLexer
  startPosition <- getPosition
  consumeCharacter
  maybeInput <- getInput
  case maybeInput of
    Nothing -> actionUnexpectedEndInString
    Just (character, _) ->
      case Map.lookup character (lexerEscapeMap lexer) of
        Just translatedCharacter -> do
          actionHelperAppendCharacterToValue translatedCharacter
          consumeCharacter
        Nothing -> do
          let handleEscapeSequence length = do
                let loop soFar i = do
                      if i == length
                        then do
                          let character = chr soFar
                          actionHelperAppendCharacterToValue character
                        else do
                          maybeInput <- getInput
                          case maybeInput of
                            Nothing -> actionUnexpectedEndInString
                            Just (character, _)
                              | Just value <- decodeHexDigit character -> do
                                consumeCharacter
                                loop (shiftL soFar 8 .|. value) (i + 1)
                              | otherwise -> do
                                error "invalid unicode hex escape codepath not implemented"
{-
                    result.message =
                      'Invalid Unicode hex escape '
                      + Unicode.showString(escapeSequence.value)
                      + ' in string.';
-}
                loop 0 0
              decodeHexDigit c
                | c >= '0' && c <= '9' = Just $ ord c - ord '0'
                | c >= 'a' && c <= 'f' = Just $ ord c - ord 'a' + 10
                | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
                | otherwise = Nothing
          case character of
            'u' -> do
              consumeCharacter
              handleEscapeSequence 4
            'U' -> do
              consumeCharacter
              handleEscapeSequence 8
            _ -> do
              consumeCharacter
              error "unexpected escape sequence type codepath not implemented"
{-
                    result.message =
                      'Unexpected escape sequence type '
                      + Unicode.showCharacter(character)
                      + ' in string.';
-}


actionHelperAppendInputToValue :: Action ()
actionHelperAppendInputToValue = do
  maybeInput <- getInput
  maybeValue <- getValue
  case (maybeValue, maybeInput) of
    (Just value, Just (character, _)) -> do
      setValue $ Just $ T.snoc value character
    _ -> return () 


actionHelperAppendCharacterToValue :: Char -> Action ()
actionHelperAppendCharacterToValue character = do
  maybeValue <- getValue
  case maybeValue of
    Just value -> setValue $ Just $ T.snoc value character
    _ -> return ()


actionStartOpenParenthesis :: Action ()
actionStartOpenParenthesis = do
  startToken
  consumeCharacter
  setStateData OpenParenthesisLexerStateData


actionFinishOpenParenthesis :: Action ()
actionFinishOpenParenthesis = do
  endToken OpenParenthesisTokenType
  setStateData TopLevelLexerStateData


actionStartCloseParenthesis :: Action ()
actionStartCloseParenthesis = do
  startToken
  consumeCharacter
  setStateData CloseParenthesisLexerStateData


actionFinishCloseParenthesis :: Action ()
actionFinishCloseParenthesis = do
  endToken CloseParenthesisTokenType
  setStateData TopLevelLexerStateData


actionParenthesisToHyphen :: Action ()
actionParenthesisToHyphen = do
  endToken CloseParenthesisTokenType
  startToken
  consumeCharacter
  setStateData HyphenLexerStateData


actionStartPeriod :: Action ()
actionStartPeriod = do
  startToken
  consumeCharacter
  setStateData PeriodLexerStateData


actionPeriodToNumber :: Action ()
actionPeriodToNumber = do
  consumeCharacter
  setStateData NumberLexerStateData


actionFinishPeriod :: Action ()
actionFinishPeriod = do
  string <- getAccumulator
  fromMaybe (do
               endPosition <- getPosition
               startPosition <-
                 getSavedPosition >>= return . fromMaybe endPosition
               let error = Error {
                               errorMessage = "Too many consecutive periods.",
                               errorSpan = Span {
                                               spanStart = startPosition,
                                               spanEnd = endPosition
                                             }
                             }
               produceError error)
            $ lookup string
                     [(".", endToken PeriodTokenType),
                      ("..", endToken OperatorTokenType),
                      ("...", endToken EllipsisTokenType)]
  setStateData TopLevelLexerStateData


actionStartWhitespace :: Action ()
actionStartWhitespace = do
  startToken
  setStateData WhitespaceLexerStateData
  consumeCharacter


actionFinishWhitespace :: Action ()
actionFinishWhitespace = do
  endPosition <- getPosition
  startPosition <- getSavedPosition >>= return . fromMaybe endPosition
  let lineCount = positionLine endPosition - positionLine startPosition
  if lineCount > 1
    then endToken ParagraphBreakTokenType
    else endToken SpaceTokenType
  setStateData TopLevelLexerStateData

