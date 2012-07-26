{-# LANGUAGE OverloadedStrings #-}
module Token
  (TokenType(..),
   Token(..))
  where

import Prelude hiding (Show(..))

import Data.Text (Text)
import qualified Data.Text as T

import Error
import Knapp.Show


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
data Token =
  Token {
      tokenType :: TokenType,
      tokenSpan :: Span,
      tokenText :: Text,
      tokenValue :: Maybe Text,
      tokenOpenDelimiter :: Maybe Char,
      tokenCloseDelimiter :: Maybe Char
    }
instance Show Token where
  show token = T.concat ["<", shownType, ">"]
    where shownType = case tokenType token of
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
