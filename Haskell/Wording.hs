{-# LANGUAGE DeriveDataTypeable #-}
module Wording
  (Wording(..),
   Section(..),
   Heading(..),
   HeadingType(..),
   Paragraph(..),
   SententialForm(..),
   Phrase(..),
   WordForm(..),
   Punctuator(..))
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
      headingType :: HeadingType,
      headingPhrase :: Phrase
    }
  deriving (Typeable)


data HeadingType
  = ChapterHeadingType {
        chapterHeadingTypeToken :: Token
      }
  | SectionHeadingType {
        sectionHeadingTypeToken :: Token
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


data Punctuator
  = Hyphen {
        hyphenToken :: Token
      }
  | Dash {
        dashToken :: Token
      }
  | Comma {
        commaToken :: Token
      }
  | Colon {
        colonToken :: Token
      }
  | Semicolon {
        semicolonToken :: Token
      }
  deriving (Typeable)

