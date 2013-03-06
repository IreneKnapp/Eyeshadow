{-# LANGUAGE OverloadedStrings #-}
module Eyeshadow.Lexical (lex) where

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Prelude as IO
import qualified System.Environment as IO

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Data.List

import Eyeshadow.Types


lex
  :: (Monad m)
  => Conduit (Char, SourcePosition)
             m
             (Either Diagnostic (SExpression, SourcePosition))
lex = do
  let readOne inParentheses quasiquotationDepth = do
        maybeItem <- peek
        case maybeItem of
          Nothing -> return ()
          Just c | Char.isDigit c ->
                     possibleNumber <- accumulateWhile Char.isDigit
                     case reads $ T.unpack possibleNumber of
                       [(value, "")] -> return $ Just $ SInteger value
                       _ -> do
                         return Nothing
                         -- TODO
                 | Char.isSpace c -> do
                     _ <- await
                     readOne inParentheses quasiquotationDepth
          Just '(' -> do
            (_, startPosition) <- await
            let loop = do
                  maybeItem <- readOne True quasiquotationDepth

