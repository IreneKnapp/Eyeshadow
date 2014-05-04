{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types,
             FlexibleContexts #-}
module Eyeshadow.Phase.Lexical
  (lex)
  where

import qualified Control.Eff as Eff
import qualified Control.Monad.Trans as Conduit
import qualified Data.Char as Char
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.Text as Text
import qualified Prelude as IO

import Control.Monad
import Data.Char
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
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Conduit.Conduit (Char, Span) (Eff.Eff r) SExpression
lex file = do
  let accumulate :: (Monad m)
                 => Conduit.ConduitM (Char, Span) b m (Text.Text, Span)
      accumulate = do
        Just (c, start) <- Conduit.await
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
                      Just (_, endSoFar) <- Conduit.await
                      let span' =
                            case concatenateSpans [spanSoFar, endSoFar] of
                              Nothing -> spanSoFar
                              Just span' -> span'
                      loop (Text.snoc textSoFar c) span'
                    else do
                      return (textSoFar, spanSoFar)
        loop (Text.singleton c) start
      readOne :: (Eff.Member Diagnose r)
              => Integer
              -> Conduit.ConduitM (Char, Span) b (Eff.Eff r) (Maybe SExpression)
      readOne quasiquotationDepth = do
        maybeItem <- Conduit.peek
        case maybeItem of
          Nothing -> return Nothing
          Just (c, span) -> do
            case classifyCharacter c of
              SpaceCharacterClassification -> do
                _ <- Conduit.await
                readOne quasiquotationDepth
              ConstituentCharacterClassification -> do
                (symbol, span) <- accumulate
                return $ Just $ SSymbol span
                  $ Name $ map NameComponent (Text.splitOn ":" symbol)
              DigitCharacterClassification -> do
                (possibleNumber, span) <- accumulate
                return $ Just $ SNumber span possibleNumber
              PunctuationCharacterClassification -> do
                case c of
                  '(' -> do
                    _ <- Conduit.await
                    items <- readMany quasiquotationDepth
                    maybeTerminator <- Conduit.await
                    case (items, maybeTerminator) of
                      ([], Nothing) -> do
                        Conduit.lift $
                          diagnoseMissingRightParenthesis file span
                        return $ Just $ SList span []
                      ([], Just (_, endSpan)) -> do
                        let span' = case concatenateSpans [span, endSpan] of
                                      Nothing -> span
                                      Just span' -> span'
                        Conduit.lift $
                          diagnoseMissingRightParenthesis file span'
                        return $ Just $ SList span' []
                      (items, Nothing) -> do
                        let span' =
                              case concatenateSpans
                                    $ [span] ++ map spanOf items of
                                Nothing -> span
                                Just span' -> span'
                        Conduit.lift $
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
                    _ <- Conduit.await
                    maybeItem <- readOne quasiquotationDepth
                    case maybeItem of
                      Just item -> do
                        let span' =
                              case concatenateSpans [span, spanOf item] of
                                Nothing -> span
                                Just span' -> span'
                        return $ Just $ SQuoted span' item
                      Nothing -> do
                        Conduit.lift $
                          diagnoseMissingQuotedExpression file span
                        return Nothing
                  '`' -> do
                    _ <- Conduit.await
                    maybeItem <- readOne (quasiquotationDepth + 1)
                    case maybeItem of
                      Just item -> do
                        let span' =
                              case concatenateSpans [span, spanOf item] of
                                Nothing -> span
                                Just span' -> span'
                        return $ Just $ SQuasiquoted span' item
                      Nothing -> do
                        Conduit.lift $
                          diagnoseMissingQuasiquotedExpression file span
                        return Nothing
                  ',' -> do
                    _ <- Conduit.await
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
                            Conduit.lift $
                              diagnoseMissingUnquotedExpression file span
                            return Nothing
                      else do
                        Conduit.lift $
                          diagnoseUnquoteNotInQuasiquote file span
                        readOne quasiquotationDepth
                  _ -> IO.error $ "Unexpected punctuator " ++ IO.show c ++ "."
      readMany :: (Eff.Member Diagnose r)
               => Integer
               -> Conduit.ConduitM (Char, Span) b (Eff.Eff r) [SExpression]
      readMany quasiquotationDepth = do
        let loop itemsSoFar = do
              maybeItem <- readOne quasiquotationDepth
              case maybeItem of
                Nothing -> return itemsSoFar
                Just item -> loop $ itemsSoFar ++ [item]
        loop []
      topLevelLoop :: (Eff.Member Diagnose r)
                   => Conduit.Conduit (Char, Span) (Eff.Eff r) SExpression
      topLevelLoop = do
        maybeItem <- readOne 0
        case maybeItem of
          Nothing -> do
            maybeTerminator <- Conduit.await
            case maybeTerminator of
              Nothing -> return ()
              Just (_, span) -> do
                Conduit.lift $ diagnoseMissingLeftParenthesis file span
                topLevelLoop
          Just item -> do
            Conduit.yield item
            topLevelLoop
  topLevelLoop


diagnoseMissingLeftParenthesis
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseMissingLeftParenthesis file span = diagnose $
  Diagnostic {
      diagnosticHeadline =
        "Right parenthesis without matching left parenthesis",
      diagnosticDescription =
        Text.concat
          ["Left and right parentheses must be kept in balance.  Don't ",
           "worry, you'll get good at counting them soon!"],
      diagnosticDetails =
        [("Closing parenthesis", file, span)]
    }


diagnoseMissingRightParenthesis
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseMissingRightParenthesis file span = diagnose $
  Diagnostic {
      diagnosticHeadline =
        "Left parenthesis without matching right parenthesis",
      diagnosticDescription =
        Text.concat
          ["Left and right parentheses must be kept in balance.  Don't ",
           "worry, you'll get good at counting them soon!"],
      diagnosticDetails =
        [("Opening parenthesis and everything it contains", file, span)]
    }


diagnoseMissingQuotedExpression
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseMissingQuotedExpression file span = diagnose $
  Diagnostic {
      diagnosticHeadline = "Quote (tick) not followed by expression",
      diagnosticDescription =
        Text.concat
          ["The quote notation is a tick (single-quote; apostrophe) followed ",
           "by the expression to be quoted.  The expression was missing in ",
           "this case, due to encountering either the end of the file or the ",
           "closing delimiter (right parenthesis) terminating the containing ",
           "expression."],
      diagnosticDetails =
        [("Offending backtick", file, span)]
    }


diagnoseMissingQuasiquotedExpression
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseMissingQuasiquotedExpression file span = diagnose $
  Diagnostic {
      diagnosticHeadline = "Quasiquote (backtick) not followed by expression",
      diagnosticDescription =
        Text.concat
          ["The quasiquote notation is a backtick followed by the expression ",
           "to be (partially) quoted.  The expression was missing in this ",
           "case, due to encountering either the end of the file or the ",
           "closing delimiter (right parenthesis) terminating the containing ",
           "expression."],
      diagnosticDetails =
        [("Offending backtick", file, span)]
    }


diagnoseMissingUnquotedExpression
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseMissingUnquotedExpression file span = diagnose $
  Diagnostic {
      diagnosticHeadline = "Unquote (comma) not followed by expression",
      diagnosticDescription =
        Text.concat
          ["The unquote notation is a comma followed by the expression to be ",
           "unquoted.  The expression was missing in this case, due to ",
           "encountering either the end of the file or the closing delimiter ",
           "(right parenthesis) terminating the containing expression."],
      diagnosticDetails =
        [("Offending comma", file, span)]
    }


diagnoseUnquoteNotInQuasiquote
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseUnquoteNotInQuasiquote file span = diagnose $
  Diagnostic {
      diagnosticHeadline = "Unquote (comma) not within quasiquote (backtick)",
      diagnosticDescription =
        Text.concat
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

