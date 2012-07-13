module Error
  (Position(..),
   Span(..),
   Error(..))
  where


data Position =
  Position {
      positionOffset :: Int,
      positionByte :: Int,
      positionLine :: Int,
      positionColumn :: Int
    }


data Span =
  Span {
      spanStart :: Position,
      spanEnd :: Position
    }


data Error =
  Error {
      errorMessage :: String,
      errorSpan :: Span
    }
