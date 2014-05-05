{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Eyeshadow.Data.Token
  (TokenType(..),
   Token(..))
  where

import qualified Data.Text as Text

import Data.Char
import Data.Maybe

import Eyeshadow.Data.Span
import Eyeshadow.Prelude


data TokenType
  = WordTokenType
  | NumberTokenType
  | StringTokenType
  | OperatorTokenType
  | PeriodTokenType
  | EllipsisTokenType
  | HyphenTokenType
  | DashTokenType
  | CommaTokenType
  | ColonTokenType
  | SemicolonTokenType
  | TickTokenType
  | BacktickTokenType
  | SpliceTokenType
  | ListSpliceTokenType
  | OpenParenthesisTokenType
  | CloseParenthesisTokenType
  | SpaceTokenType
  | ParagraphBreakTokenType
  deriving (Eq)


data Token =
  Token {
      tokenType :: TokenType,
      tokenSpan :: Span,
      tokenText :: Text.Text,
      tokenValue :: Maybe Text.Text,
      tokenOpenDelimiter :: Maybe Char,
      tokenCloseDelimiter :: Maybe Char
    }
instance HasSpan Token where
  spanOf Token{..} = tokenSpan
instance Show Token where
  show Token{..} =
    case tokenType of
      WordTokenType -> "word"
      NumberTokenType -> "number"
      StringTokenType -> "string"
      OperatorTokenType -> "operator"
      PeriodTokenType -> "period"
      EllipsisTokenType -> "ellipsis"
      HyphenTokenType -> "hyphen"
      DashTokenType -> "dash"
      CommaTokenType -> "comma"
      ColonTokenType -> "colon"
      SemicolonTokenType -> "semicolon"
      TickTokenType -> "tick"
      BacktickTokenType -> "backtick"
      SpliceTokenType -> "splice"
      ListSpliceTokenType -> "list-splice"
      OpenParenthesisTokenType -> "open-parenthesis"
      CloseParenthesisTokenType -> "close-parenthesis"
      SpaceTokenType -> "space"
      ParagraphBreakTokenType -> "paragraph-break"
