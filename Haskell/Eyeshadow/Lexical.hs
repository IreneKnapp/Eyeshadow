{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Eyeshadow.Lexical (lex) where

import qualified Data.Char as Char
import qualified Data.Conduit.List as Conduit
import qualified Data.Text as T
import qualified Prelude as IO

import Control.Monad
import Data.Char
import Data.Conduit
import Data.Either
import Data.Eq
import Data.List
import Data.Ord
import Data.Maybe
import Prelude (($), (.), Num(..), Integer)

import Eyeshadow.Spans
import Eyeshadow.Types


data CharacterClassification
  = DigitCharacterClassification
  | ConstituentCharacterClassification
  | PunctuationCharacterClassification
  | SpaceCharacterClassification
  deriving (Eq)


classifyCharacter :: Char -> CharacterClassification
classifyCharacter c | Char.isDigit c = DigitCharacterClassification
classifyCharacter c | Char.isSpace c = SpaceCharacterClassification
classifyCharacter c | elem c "()'`,#" = PunctuationCharacterClassification
classifyCharacter _ = ConstituentCharacterClassification


lex
  :: (Monad m)
  => SourceFileSpecification
  -> Conduit (Char, SourcePosition) m (Either Diagnostic SExpression)
lex file = do
  let accumulate :: (Monad m)
                 => ConduitM (Char, SourcePosition)
                             (Either Diagnostic b)
                             m
                             (T.Text, SourceSpan)
      accumulate = do
        Just (c, start) <- await
        let loop textSoFar endSoFar = do
              maybeItem <- Conduit.peek
              case maybeItem of
                Nothing ->
                  return (textSoFar, simpleSpan start endSoFar)
                Just (c, position) ->
                  if elem (classifyCharacter c)
                          [DigitCharacterClassification,
                           ConstituentCharacterClassification]
                    then do
                      _ <- await
                      loop (T.snoc textSoFar c) position
                    else
                      return (textSoFar, simpleSpan start endSoFar)
        loop (T.singleton c) start
      readOne :: (Monad m)
              => Integer
              -> ConduitM (Char, SourcePosition)
                          (Either Diagnostic b)
                          m
                          (Maybe SExpression)
      readOne quasiquotationDepth = do
        maybeItem <- Conduit.peek
        case maybeItem of
          Nothing -> return Nothing
          Just (c, position) -> do
            case classifyCharacter c of
              SpaceCharacterClassification -> do
                _ <- await
                readOne quasiquotationDepth
              ConstituentCharacterClassification -> do
                (symbol, span) <- accumulate
                return $ Just $ SSymbol span (T.splitOn ":" symbol)
              DigitCharacterClassification -> do
                (possibleNumber, span) <- accumulate
                return $ Just $ SNumber span possibleNumber
              PunctuationCharacterClassification -> do
                case c of
                  '(' -> do
                    _ <- await
                    items <- readMany quasiquotationDepth
                    maybeTerminator <- await
                    case (items, maybeTerminator) of
                      ([], Nothing) -> do
                        let span = singletonSpan position
                        diagnoseMissingRightParenthesis file span
                        return $ Just $ SList span []
                      ([], Just (_, end)) -> do
                        let span = simpleSpan position end
                        diagnoseMissingRightParenthesis file span
                        return $ Just $ SList span []
                      (items, Nothing) -> do
                        let span =
                              case concatenateSpans
                                    $ map expressionSpan items of
                                Nothing -> singletonSpan position
                                Just span' -> extendSpanBackward position span'
                        diagnoseMissingRightParenthesis file span
                        return $ Just $ SList span items
                      (items, Just (_, end)) -> do
                        return $ Just $ SList (simpleSpan position end) items
                  ')' -> return Nothing
                  '\'' -> do
                    _ <- await
                    maybeItem <- readOne quasiquotationDepth
                    case maybeItem of
                      Just item -> do
                        let span = extendSpanBackward position
                                                      (expressionSpan item)
                        return $ Just $ SQuoted span item
                      Nothing -> do
                        let span = singletonSpan position
                        diagnoseMissingQuotedExpression file span
                        return Nothing
                  '`' -> do
                    _ <- await
                    maybeItem <- readOne (quasiquotationDepth + 1)
                    case maybeItem of
                      Just item -> do
                        let span = extendSpanBackward position
                                                      (expressionSpan item)
                        return $ Just $ SQuasiquoted span item
                      Nothing -> do
                        let span = singletonSpan position
                        diagnoseMissingQuasiquotedExpression file span
                        return Nothing
                  ',' -> do
                    _ <- await
                    if quasiquotationDepth > 0
                      then do
                        maybeItem <- readOne (quasiquotationDepth - 1)
                        case maybeItem of
                          Just item -> do
                            let span = extendSpanBackward position
                                                          (expressionSpan item)
                            return $ Just $ SAntiquoted span item
                          Nothing -> do
                            let span = singletonSpan position
                            diagnoseMissingUnquotedExpression file span
                            return Nothing
                      else do
                        let span = singletonSpan position
                        diagnoseUnquoteNotInQuasiquote file span
                        readOne quasiquotationDepth
                  _ -> IO.error $ "Unexpected punctuator " ++ IO.show c ++ "."
      readMany :: (Monad m)
               => Integer
               -> ConduitM (Char, SourcePosition)
                           (Either Diagnostic b)
                           m
                           [SExpression]
      readMany quasiquotationDepth = do
        let loop itemsSoFar = do
              maybeItem <- readOne quasiquotationDepth
              case maybeItem of
                Nothing -> return itemsSoFar
                Just item -> loop $ itemsSoFar ++ [item]
        loop []
      topLevelLoop :: (Monad m)
                   => Conduit (Char, SourcePosition)
                              m
                              (Either Diagnostic SExpression)
      topLevelLoop = do
        maybeItem <- readOne 0
        case maybeItem of
          Nothing -> do
            maybeTerminator <- await
            case maybeTerminator of
              Nothing -> return ()
              Just (_, position) -> do
                let span = SourceSpan {
                               sourceSpanStart = position,
                               sourceSpanEnd = position
                             }
                diagnoseMissingLeftParenthesis file span
                topLevelLoop
          Just item -> do
            yield $ Right item
            topLevelLoop
  topLevelLoop


diagnoseMissingLeftParenthesis
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseMissingLeftParenthesis file span = yield $ Left $
  Diagnostic {
      diagnosticHeadline =
        "Right parenthesis without matching left parenthesis",
      diagnosticDescription =
        T.concat
          ["Left and right parentheses must be kept in balance.  Don't ",
           "worry, you'll get good at counting them soon!"],
      diagnosticDetails =
        [("Closing parenthesis", file, span)]
    }


diagnoseMissingRightParenthesis
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseMissingRightParenthesis file span = yield $ Left $
  Diagnostic {
      diagnosticHeadline =
        "Left parenthesis without matching right parenthesis",
      diagnosticDescription =
        T.concat
          ["Left and right parentheses must be kept in balance.  Don't ",
           "worry, you'll get good at counting them soon!"],
      diagnosticDetails =
        [("Opening parenthesis and everything it contains", file, span)]
    }


diagnoseMissingQuotedExpression
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseMissingQuotedExpression file span = yield $ Left $
  Diagnostic {
      diagnosticHeadline = "Quote (tick) not followed by expression",
      diagnosticDescription =
        T.concat
          ["The quote notation is a tick (single-quote; apostrophe) followed ",
           "by the expression to be quoted.  The expression was missing in ",
           "this case, due to encountering either the end of the file or the ",
           "closing delimiter (right parenthesis) terminating the containing ",
           "expression."],
      diagnosticDetails =
        [("Offending backtick", file, span)]
    }


diagnoseMissingQuasiquotedExpression
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseMissingQuasiquotedExpression file span = yield $ Left $
  Diagnostic {
      diagnosticHeadline = "Quasiquote (backtick) not followed by expression",
      diagnosticDescription =
        T.concat
          ["The quasiquote notation is a backtick followed by the expression ",
           "to be (partially) quoted.  The expression was missing in this ",
           "case, due to encountering either the end of the file or the ",
           "closing delimiter (right parenthesis) terminating the containing ",
           "expression."],
      diagnosticDetails =
        [("Offending backtick", file, span)]
    }


diagnoseMissingUnquotedExpression
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseMissingUnquotedExpression file span = yield $ Left $
  Diagnostic {
      diagnosticHeadline = "Unquote (comma) not followed by expression",
      diagnosticDescription =
        T.concat
          ["The unquote notation is a comma followed by the expression to be ",
           "unquoted.  The expression was missing in this case, due to ",
           "encountering either the end of the file or the closing delimiter ",
           "(right parenthesis) terminating the containing expression."],
      diagnosticDetails =
        [("Offending comma", file, span)]
    }


diagnoseUnquoteNotInQuasiquote
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseUnquoteNotInQuasiquote file span = yield $ Left $
  Diagnostic {
      diagnosticHeadline = "Unquote (comma) not within quasiquote (backtick)",
      diagnosticDescription =
        T.concat
          ["The quasiquotation mechanism allows evaluated snippets ",
           "to be mixed into a spine of structure which does not get ",
           "evaluated.  It only makes sense to use the punctuation that ",
           "designates the following expression is to be unquoted when ",
           "already within the expression that follows a quasiquote.  If ",
           "you think that is the case, try counting commas and backticks ",
           "and making sure that there are no more nested commas than there ",
           "are nested backticks."],
      diagnosticDetails =
        [("Offending comma", file, span)]
    }

