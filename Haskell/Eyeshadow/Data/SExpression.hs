{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Eyeshadow.Data.SExpression
  (SExpression(..))
  where

import qualified Data.Text as T

import Data.List

import Eyeshadow.Data.Span
import Eyeshadow.Prelude


data SExpression
  = SNumber Span T.Text
  | SString Span T.Text
  | SSymbol Span [T.Text]
  | SList Span [SExpression]
  | SQuoted Span SExpression
  | SQuasiquoted Span SExpression
  | SAntiquoted Span SExpression
instance Show SExpression where
  show (SNumber _ int) = int
  show (SString _ string) =
    T.concat ["\"",
              T.foldl' (\soFar c ->
                          T.concat [soFar,
                                    case c of
                                      '"' -> "\"\""
                                      _ -> T.singleton c])
                       string
              "\""]
  show (SSymbol _ parts) = T.intercalate ":" parts
  show (SList _ items) =
    T.concat ["(", T.intercalate " " $ map show items, ")"]
  show (SQuoted _ item) = T.concat ["'", show item]
  show (SQuasiquoted _ item) = T.concat ["`", show item]
  show (SAntiquoted _ item) = T.concat [",", show item]
instance HasSpan SExpression where
  spanOf (SNumber span _) = span
  spanOf (SString span _) = span
  spanOf (SSymbol span _) = span
  spanOf (SList span _) = span
  spanOf (SQuoted span _) = span
  spanOf (SQuasiquoted span _) = span
  spanOf (SAntiquoted span _) = span
