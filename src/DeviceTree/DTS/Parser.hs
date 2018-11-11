-- |
{-# LANGUAGE OverloadedStrings #-}

module DeviceTree.DTS.Parser where

import qualified Data.Char                  (isDigit, ord)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, takeWhile1P)
import           Text.Megaparsec.Char       (satisfy)
import qualified Text.Megaparsec.Char.Lexer as L

-- TODO: Move types out of this module once I figure out what they are.

newtype Label = Label Text deriving (Eq, Show)

type Parser = Parsec Void Text

label :: Parser Label
label = undefined
  where
    isFirstChar :: Char -> Bool
    isFirstChar = anyOf [isLowerCaseLetter, isUpperCaseLetter, isUnderscore]

    isLabelChar :: Char -> Bool
    isLabelChar = anyOf [ isLowerCaseLetter
                        , isUpperCaseLetter
                        , isDigit
                        , isUnderscore ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
  where
    spaceConsumer, lineComment, blockComment, whiteSpace :: Parser ()

    spaceConsumer = L.space whiteSpace lineComment blockComment
    lineComment   = L.skipLineComment "//"
    blockComment  = L.skipBlockComment "/*" "*/"
    whiteSpace    = takeWhile1P (Just "whitespace") isWhitespace >> pure ()


-- | Apply a 'Traversable' of functions to a value, returning True if any of
--   them a True.
anyOf :: Traversable t => t (a -> Bool) -> a -> Bool
anyOf fs x = or $ fmap (\f -> f x) fs

-------------------------------------------------------------------------------
-- Character Class Predicates
-------------------------------------------------------------------------------

-- | Underscore character.
isUnderscore :: Char -> Bool
isUnderscore = (==) '_'

-- | ASCII digit '0' - '9'.
isDigit :: Char -> Bool
isDigit = Data.Char.isDigit

-- | Space, tabs, newlines.
isWhitespace :: Char -> Bool
isWhitespace c =
    (c == ' ')  ||
    (c == '\t') ||
    (c == '\n') ||
    (c == '\r')

-- | ASCII lowercase letter 'a' - 'z'.
isLowerCaseLetter :: Char -> Bool
isLowerCaseLetter c = c' >= Data.Char.ord 'a' && c' <= Data.Char.ord 'z'
  where c' = Data.Char.ord c

-- | ASCII uppercase letter 'A' - 'Z'.
isUpperCaseLetter :: Char -> Bool
isUpperCaseLetter c = c' >= Data.Char.ord 'A' && c' <= Data.Char.ord 'Z'
  where c' = Data.Char.ord c
