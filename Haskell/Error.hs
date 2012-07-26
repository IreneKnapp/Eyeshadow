{-# LANGUAGE OverloadedStrings #-}
module Error
  (Position(..),
   Span(..),
   Error(..))
  where

import Prelude hiding (Show(..))

import Data.Text (Text)
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
  show position = T.concat [show $ positionLine position, ":",
                            show $ positionColumn position,
                            "(", show $ positionCharacter position, ")"]


data Span =
  Span {
      spanStart :: Position,
      spanEnd :: Position
    }
instance Show Span where
  show span = T.concat [show $ spanStart span, "-", show $ spanEnd span]


data Error =
  Error {
      errorMessage :: Text,
      errorSpan :: Span
    }
instance Show Error where
  show error = T.concat [show $ errorSpan error, "\n", errorMessage error]

