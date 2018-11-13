-- |
{-# LANGUAGE OverloadedStrings #-}

module DeviceTree.DTS.Lexer where

import Data.Char (isPrint)
import           Data.Functor         (($>))
import qualified Data.List.NonEmpty   as NE
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, Pos, SourcePos, getParserState,
                                       many, manyTill, parseTest, sourceColumn,
                                       sourceLine, sourceName, statePos,
                                       takeWhile1P, (<|>))
import           Text.Megaparsec.Char (anyChar, string, char, satisfy)


quickTest :: IO ()
quickTest = do
    let input = "/* This is a comment *///This is another comment\n#include \"Hello\""
    parseTest posTokenList input


type Parser = Parsec Void Text


data Position
    = Position
      { positionSource :: !Text
      , positionLine   :: !Pos
      , positionColumn :: !Pos
      }
      deriving (Show)


sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition sp
    = Position
      { positionSource = T.pack . sourceName $ sp
      , positionLine   = sourceLine sp
      , positionColumn = sourceColumn sp
      }


data Positioned a = Positioned Position a deriving (Show)


positioned :: Parser a -> Parser (Positioned a)
positioned parser = do
    state <- getParserState
    let position = sourcePosToPosition . NE.head . statePos $ state
    Positioned position <$> parser


type PosToken = Positioned Token


posToken :: Parser PosToken
posToken = positioned token


posTokenList :: Parser [PosToken]
posTokenList = many posToken


data Token
    = TokInclude Include
    | TokComment Comment
    | TokWhitespace Whitespace
    | TokLiteralString LiteralString
    deriving (Show)


token :: Parser Token
token
    = TokInclude       <$> include
  <|> TokComment       <$> comment
  <|> TokWhitespace    <$> whitespace
  <|> TokLiteralString <$> literalString


data Include
    = IncludeCStyle
    | IncludeDTStyle
    deriving (Show)


include :: Parser Include
include
    = (string "#include"  $> IncludeCStyle)
  <|> (string "/include/" $> IncludeDTStyle)


data Comment
    = CommentBlock Text
    | CommentLine Text
    deriving (Show)


comment :: Parser Comment
comment
    = blockComment
  <|> lineComment

  where

    blockComment
        = CommentBlock
      <$> ( string "/*"
         *> (T.pack <$> manyTill anyChar (string "*/")) )

    lineComment
        = CommentLine
      <$> ( string "//"
         *> takeWhile1P (Just "character") (/= '\n') )


newtype Whitespace = Whitespace Text deriving (Show)


whitespace :: Parser Whitespace
whitespace = Whitespace <$> takeWhile1P (Just "whitespace") isWhitespace
  where
    isWhitespace c = case c of
        ' '  -> True
        '\t' -> True
        '\n' -> True
        '\r' -> True
        _    -> False


newtype LiteralString = LiteralString Text deriving (Show)


literalString :: Parser LiteralString
literalString = LiteralString <$> (char '"' *> stringContents)
  where
    stringContents :: Parser Text
    stringContents = T.concat <$> manyTill stringAtom (char '"')

    stringAtom :: Parser Text
    stringAtom
        = T.singleton <$> satisfy isStringChar
      <|> string "\\\""

    isStringChar :: Char -> Bool
    isStringChar = isPrint
