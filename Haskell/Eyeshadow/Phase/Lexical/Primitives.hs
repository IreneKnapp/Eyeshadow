{-# LANGUAGE NoImplicitPrelude, RecordWildCards, FlexibleContexts,
             ImpredicativeTypes, OverloadedStrings #-}
module Eyeshadow.Phase.Lexical.Primitives
  (consumeCharacter,
   startToken,
   endToken,
   setStateData,
   getLexer,
   getInput,
   getAccumulator,
   setValue,
   getValue,
   setOpenDelimiter,
   getOpenDelimiter,
   setCloseDelimiter,
   getCloseDelimiter,
   done)
  where

import qualified Control.Eff.Reader.Strict as Eff
import qualified Control.Eff.State.Strict as Eff
import qualified Control.Monad.Trans as Conduit
import qualified Data.Conduit as Conduit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Data.Char
import Data.Functor
import Data.Maybe

import Eyeshadow.Data.Span
import Eyeshadow.Data.Token
import Eyeshadow.Data.Unicode
import Eyeshadow.Phase.Lexical.Types
import Eyeshadow.Prelude


classify :: Lexer -> Char -> Classification
classify lexer character =
  let classificationMap = lexerClassificationMap lexer
      minorCategoryMap = lexerMinorCategoryMap lexer
      majorCategoryMap = lexerMajorCategoryMap lexer
      minorCategory = fromMaybe "??" $ categorize character
      majorCategory = Text.take 1 minorCategory
      maybeDirect = HashMap.lookup character classificationMap
      maybeMinorCategoryBased = HashMap.lookup minorCategory minorCategoryMap
      maybeMajorCategoryBased = HashMap.lookup majorCategory majorCategoryMap
  in case maybeDirect of
       Just result -> result
       Nothing -> case maybeMinorCategoryBased of
                    Just result -> result
                    Nothing -> case maybeMajorCategoryBased of
                                 Just result -> result
                                 Nothing -> UnknownClassification


consumeCharacter :: Action ()
consumeCharacter = do
  lexer <- Conduit.lift Eff.ask
  state@LexerState{..} <- Conduit.lift Eff.get
  maybeNewCharacterAndSpan <- Conduit.await
  let accumulator =
        case lexerStateInput of
          Nothing -> lexerStateAccumulator
          Just (characterHere, _, _) ->
            Text.snoc lexerStateAccumulator characterHere
      span =
        case lexerStateInput of
          Nothing -> lexerStateSpan
          Just (_, spanHere, _) ->
            case lexerStateSpan of
              Nothing -> Just spanHere
              Just spanBefore -> concatenateSpans [spanBefore, spanHere]
      input = fmap (\(newCharacter, newSpan) ->
                      (newCharacter, newSpan, classify lexer newCharacter))
                   maybeNewCharacterAndSpan
      state' = state {
                    lexerStateSpan = span,
                    lexerStateAccumulator = accumulator,
                    lexerStateInput = input
                  }
  Conduit.lift $ Eff.put state'


startToken :: Action ()
startToken = Conduit.lift $ do
  state <- Eff.get
  Eff.put $ state {
                lexerStateSpan = Nothing,
                lexerStateAccumulator = Text.empty,
                lexerStateValue = Nothing,
                lexerStateOpenDelimiter = Nothing,
                lexerStateCloseDelimiter = Nothing
              }


endToken :: TokenType -> Action ()
endToken tokenType = do
  state@LexerState{..} <- Conduit.lift Eff.get
  case lexerStateSpan of
    Nothing -> return ()
    Just span -> do
      let text = lexerStateAccumulator
          value = lexerStateValue
          openDelimiter = lexerStateOpenDelimiter
          closeDelimiter = lexerStateCloseDelimiter
      Conduit.lift $ Eff.put $ state {
                                   lexerStateSpan = Nothing,
                                   lexerStateAccumulator = Text.empty,
                                   lexerStateValue = Nothing,
                                   lexerStateOpenDelimiter = Nothing,
                                   lexerStateCloseDelimiter = Nothing
                                 }
      Conduit.yield $ Token {
                          tokenType = tokenType,
                          tokenSpan = span,
                          tokenText = text,
                          tokenValue = value,
                          tokenOpenDelimiter = openDelimiter,
                          tokenCloseDelimiter = closeDelimiter
                        }


setStateData :: LexerStateData -> Action ()
setStateData stateData = Conduit.lift $ do
  state <- Eff.get
  Eff.put $ state {
                lexerStateData = stateData
              }


getLexer :: Action Lexer
getLexer = Conduit.lift $ do
  Eff.ask


getInput :: Action (Maybe Char)
getInput = Conduit.lift $ do
  LexerState{..} <- Eff.get
  case lexerStateInput of
    Nothing -> return Nothing
    Just (character, _, _) -> return $ Just character


getAccumulator :: Action Text.Text
getAccumulator = Conduit.lift $ do
  LexerState{..} <- Eff.get
  return lexerStateAccumulator


setValue :: Maybe Text.Text -> Action ()
setValue value = Conduit.lift $ do
  state <- Eff.get
  Eff.put $ state {
                lexerStateValue = value
              }


getValue :: Action (Maybe Text.Text)
getValue = Conduit.lift $ do
  LexerState{..} <- Eff.get
  return lexerStateValue


setOpenDelimiter :: Maybe Char -> Action ()
setOpenDelimiter value = Conduit.lift $ do
  state <- Eff.get
  Eff.put $ state {
                lexerStateOpenDelimiter = value
              }


getOpenDelimiter :: Action (Maybe Char)
getOpenDelimiter = Conduit.lift $ do
  LexerState{..} <- Eff.get
  return lexerStateOpenDelimiter


setCloseDelimiter :: Maybe Char -> Action ()
setCloseDelimiter value = Conduit.lift $ do
  state <- Eff.get
  Eff.put $ state {
                lexerStateCloseDelimiter = value
              }


getCloseDelimiter :: Action (Maybe Char)
getCloseDelimiter = Conduit.lift $ do
  LexerState{..} <- Eff.get
  return lexerStateCloseDelimiter


done :: Action ()
done = Conduit.lift $ do
  state <- Eff.get
  Eff.put $ state {
                lexerStateDone = True
              }
