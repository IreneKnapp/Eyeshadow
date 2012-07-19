{-# LANGUAGE OverloadedStrings #-}
module Error
  (Position(..),
   Span(..),
   Error(..))
  where

import Prelude hiding (Show(..))

import qualified Data.Text as T

import Knapp.Show


data Position =
  Position {
      positionCharacter :: Int,
      positionByte :: Int,
      positionLine :: Int,
      positionColumn :: Int
    }
instance Show Position where
  show position = T.concat ["line ", show $ positionLine position, ", ",
                            "column ", show $ positionColumn position]


data Span =
  Span {
      spanStart :: Position,
      spanEnd :: Position
    }
instance Show Span where
  show span = T.concat [show $ spanStart span,
                        " through ", show $ spanEnd span]


data Error =
  Error {
      errorMessage :: String,
      errorSpan :: Span
    }

