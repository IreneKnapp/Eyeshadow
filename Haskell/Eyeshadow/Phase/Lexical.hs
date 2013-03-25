{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types #-}
module Eyeshadow.Phase.Lexical
  (lex)
  where

import qualified Data.Char as Char
import qualified Data.Conduit.List as Conduit
import qualified Data.Text as T
import qualified Prelude as IO

import Control.Monad
import Data.Char
import Data.Conduit
import Data.Eq
import Data.List
import Data.Ord
import Data.Maybe

import Eyeshadow.Diagnostic
import Eyeshadow.Data.Name
import Eyeshadow.Data.SExpression
import Eyeshadow.Data.Span
import Eyeshadow.Prelude


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
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Conduit (Char, Span) m SExpression
lex file = do
  let accumulate :: (Monad m)
                 => ConduitM (Char, Span) b m (T.Text, Span)
      accumulate = do
        Just (c, start) <- await
        let loop textSoFar spanSoFar = do
              maybeItem <- Conduit.peek
              case maybeItem of
                Nothing -> do
                  return (textSoFar, spanSoFar)
                Just (c, _) ->
                  if elem (classifyCharacter c)
                          [DigitCharacterClassification,
                           ConstituentCharacterClassification]
                    then do
                      Just (_, endSoFar) <- await
                      let span' =
                            case concatenateSpans [spanSoFar, endSoFar] of
                              Nothing -> spanSoFar
                              Just span' -> span'
                      loop (T.snoc textSoFar c) span'
                    else do
                      return (textSoFar, spanSoFar)
        loop (T.singleton c) start
      readOne :: (MonadDiagnostic m)
              => Integer
              -> ConduitM (Char, Span) b m (Maybe SExpression)
      readOne quasiquotationDepth = do
        maybeItem <- Conduit.peek
        case maybeItem of
          Nothing -> return Nothing
          Just (c, span) -> do
            case classifyCharacter c of
              SpaceCharacterClassification -> do
                _ <- await
                readOne quasiquotationDepth
              ConstituentCharacterClassification -> do
                (symbol, span) <- accumulate
                return $ Just $ SSymbol span
                  $ Name $ map NameComponent (T.splitOn ":" symbol)
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
                        diagnoseMissingRightParenthesis file span
                        return $ Just $ SList span []
                      ([], Just (_, endSpan)) -> do
                        let span' = case concatenateSpans [span, endSpan] of
                                      Nothing -> span
                                      Just span' -> span'
                        diagnoseMissingRightParenthesis file span'
                        return $ Just $ SList span' []
                      (items, Nothing) -> do
                        let span' =
                              case concatenateSpans
                                    $ [span] ++ map spanOf items of
                                Nothing -> span
                                Just span' -> span'
                        diagnoseMissingRightParenthesis file span'
                        return $ Just $ SList span' items
                      (items, Just (_, endSpan)) -> do
                        let span' =
                              case concatenateSpans
                                    $ [span]
                                      ++ map spanOf items
                                      ++ [endSpan] of
                                Nothing -> span
                                Just span' -> span'
                        return $ Just $ SList span' items
                  ')' -> return Nothing
                  '\'' -> do
                    _ <- await
                    maybeItem <- readOne quasiquotationDepth
                    case maybeItem of
                      Just item -> do
                        let span' =
                              case concatenateSpans [span, spanOf item] of
                                Nothing -> span
                                Just span' -> span'
                        return $ Just $ SQuoted span' item
                      Nothing -> do
                        diagnoseMissingQuotedExpression file span
                        return Nothing
                  '`' -> do
                    _ <- await
                    maybeItem <- readOne (quasiquotationDepth + 1)
                    case maybeItem of
                      Just item -> do
                        let span' =
                              case concatenateSpans [span, spanOf item] of
                                Nothing -> span
                                Just span' -> span'
                        return $ Just $ SQuasiquoted span' item
                      Nothing -> do
                        diagnoseMissingQuasiquotedExpression file span
                        return Nothing
                  ',' -> do
                    _ <- await
                    if quasiquotationDepth > 0
                      then do
                        maybeItem <- readOne (quasiquotationDepth - 1)
                        case maybeItem of
                          Just item -> do
                            let span' =
                                  case concatenateSpans [span, spanOf item] of
                                    Nothing -> span
                                    Just span' -> span'
                            return $ Just $ SAntiquoted span' item
                          Nothing -> do
                            diagnoseMissingUnquotedExpression file span
                            return Nothing
                      else do
                        diagnoseUnquoteNotInQuasiquote file span
                        readOne quasiquotationDepth
                  _ -> IO.error $ "Unexpected punctuator " ++ IO.show c ++ "."
      readMany :: (MonadDiagnostic m)
               => Integer
               -> ConduitM (Char, Span) b m [SExpression]
      readMany quasiquotationDepth = do
        let loop itemsSoFar = do
              maybeItem <- readOne quasiquotationDepth
              case maybeItem of
                Nothing -> return itemsSoFar
                Just item -> loop $ itemsSoFar ++ [item]
        loop []
      topLevelLoop :: (MonadDiagnostic m)
                   => Conduit (Char, Span) m SExpression
      topLevelLoop = do
        maybeItem <- readOne 0
        case maybeItem of
          Nothing -> do
            maybeTerminator <- await
            case maybeTerminator of
              Nothing -> return ()
              Just (_, span) -> do
                diagnoseMissingLeftParenthesis file span
                topLevelLoop
          Just item -> do
            yield item
            topLevelLoop
  topLevelLoop


diagnoseMissingLeftParenthesis
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseMissingLeftParenthesis file span = diagnose $
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
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseMissingRightParenthesis file span = diagnose $
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
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseMissingQuotedExpression file span = diagnose $
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
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseMissingQuasiquotedExpression file span = diagnose $
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
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseMissingUnquotedExpression file span = diagnose $
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
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseUnquoteNotInQuasiquote file span = diagnose $
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

