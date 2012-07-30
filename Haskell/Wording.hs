{-# LANGUAGE DeriveDataTypeable #-}
module Wording
  (Wording(..),
   Section(..),
   Heading(..),
   Paragraph(..),
   SententialForm(..),
   Phrase(..),
   WordForm(..))
  where

import Data.Typeable

import Token


data Wording =
  Toplevel {
      toplevelIntroduction :: [Paragraph],
      toplevelSections :: [Section]
    }
  deriving (Typeable)


data Section =
  Section {
      sectionHeading :: Heading,
      sectionBody :: [Paragraph]
    }
  deriving (Typeable)


data Heading =
  Heading {
      headingIntroducer :: Token,
      headingPhrase :: Phrase
    }
  deriving (Typeable)


data Paragraph =
  Paragraph {
      paragraphBody :: [SententialForm]
    }
  deriving (Typeable)


data SententialForm
  = SententialParenthetical {
        sententialParentheticalBody :: [SententialForm],
        sententialParentheticalOpenDelimiter :: Token,
        sententialParentheticalCloseDelimiter :: Token
      }
  | Sentence {
        sentenceBody :: Phrase,
        sentenceCloseDelimiter :: Token
      }
  deriving (Typeable)


data Phrase =
  Phrase {
      phraseWords :: [WordForm],
      phrasePunctuators :: [Maybe Token]
    }
  deriving (Typeable)


data WordForm
  = Word {
        wordToken :: Token
      }
  | Number {
        numberToken :: Token
      }
  | Operator {
        operatorToken :: Token
      }
  | String {
        stringToken :: Token
      }
  | WordParenthetical {
        wordParentheticalBody :: Phrase,
        wordParentheticalOpenDelimiter :: Token,
        wordParentheticalCloseDelimiter :: Token
      }
  | QuotedWord {
        quotedWordBody :: WordForm,
        quotedWordOpenDelimiter :: Token
      }
  | QuasiquotedWord {
        quasiquotedWordBody :: WordForm,
        quasiquotedWordOpenDelimiter :: Token
      }
  | SplicedWord {
        splicedWordBody :: WordForm,
        splicedWordOpenDelimiter :: Token
      }
  | ListSplicedWord {
        listSplicedWordBody :: WordForm,
        listSplicedWordOpenDelimiter :: Token
      }
  deriving (Typeable)

