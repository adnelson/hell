{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | Parser for the Hell language.

module Hell.Parser
  ( parseQuotedByteString
  , parseQuoted
  ) where

import           CaseOf
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Void
import           Hell.Lexer
import           Hell.Types
import           System.Exit
import qualified Text.Megaparsec as Mega

-- | Parse (Located Token)s into an AST.
type Parser = Mega.Parsec Void LTokens

--------------------------------------------------------------------------------
-- Quoted code parsers

-- | Parse a quoted string like @ls -alh@.
parseQuotedByteString ::
     FilePath -> ByteString -> Either String SomeAction
parseQuotedByteString fp bs =
  case lexQuotedByteString fp bs of
    Left e -> Left e
    Right toks ->
      case parseQuoted fp (LTokens toks) of
        Right k -> pure k
        Left e -> Left (Mega.errorBundlePretty e)

-- | Parse a quoted set of tokens like @ls -ali@.
parseQuoted ::
     FilePath
  -> LTokens
  -> Either (Mega.ParseErrorBundle LTokens Void) SomeAction
parseQuoted fp toks = Mega.parse actionParser fp toks

-- | A parser for some action pipeline.
actionParser :: Parser SomeAction
actionParser = do
  sequenceParser

-- | Parser for a sequence of commands e.g. @x | y | z; second; third@.
sequenceParser :: Parser SomeAction
sequenceParser = do
  x <- pipeParser
  sequenced x <|> backgrounded x <|> pure (SomeAction x)
  where
    sequenced x =
      semiParser *>
      (do y <- sequenceParser
          case y of
            SomeAction y' -> pure (SomeAction (Sequence x y')))
    backgrounded x = ampersandParser *> pure (SomeAction (Background x))

-- | Parser for a pipe of commands e.g. @x | y | z@.
pipeParser :: Parser (Action ExitCode)
pipeParser = do
  x <- commandParser
  xs <- Mega.many (barParser *> commandParser)
  pure (foldl Pipe x xs)

-- | Parser for a command e.g. @ls -alh@.
commandParser :: Parser (Action ExitCode)
commandParser = do
  cmd <- fmap locatedThing quotedParser
  args <- Mega.many quotedParser
  case cmd of
    "cd" ->
      case args of
        [] -> pure (ChangeDirectory GoHome)
        [pwd] -> pure (ChangeDirectory (Chdir (T.unpack (locatedThing pwd))))
        _ -> fail "cd: too many arguments"
    _ -> pure (Command cmd (map locatedThing (toList args)))

--------------------------------------------------------------------------------
-- Token parser combinators

-- spliceBeginParser :: Parser (Located ())
-- spliceBeginParser = expect $(maybeCaseOf 'SpliceBeginToken)

-- spliceEndParser :: Parser (Located ())
-- spliceEndParser = expect $(maybeCaseOf 'SpliceEndToken)

-- spliceVarParser :: Parser (Located ByteString)
-- spliceVarParser = expect $(maybeCaseOf 'SpliceVarToken)

-- quoteBeginParser :: Parser (Located ())
-- quoteBeginParser = expect $(maybeCaseOf 'QuoteBeginToken)

-- quoteEndParser :: Parser (Located ())
-- quoteEndParser = expect $(maybeCaseOf 'QuoteEndToken)

quotedParser :: Parser (Located Text)
quotedParser = expect $(maybeCaseOf 'QuotedToken) >>= utf8Text

-- subBeginParser :: Parser (Located ())
-- subBeginParser = expect $(maybeCaseOf 'SubBeginToken)

-- subEndParser :: Parser (Located ())
-- subEndParser = expect $(maybeCaseOf 'SubEndToken)

-- stringLiteralParser :: Parser (Located ByteString)
-- stringLiteralParser = expect $(maybeCaseOf 'StringLiteralToken)

-- commentParser :: Parser (Located ByteString)
-- commentParser = expect $(maybeCaseOf 'CommentToken)

-- lowerWordParser :: Parser (Located ByteString)
-- lowerWordParser = expect $(maybeCaseOf 'LowerWordToken)

-- equalsParser :: Parser (Located ())
-- equalsParser = expect $(maybeCaseOf 'EqualsToken)

-- openBracketParser :: Parser (Located ())
-- openBracketParser = expect $(maybeCaseOf 'OpenBracketToken)

-- closeBracketParser :: Parser (Located ())
-- closeBracketParser = expect $(maybeCaseOf 'CloseBracketToken)

-- openParenParser :: Parser (Located ())
-- openParenParser = expect $(maybeCaseOf 'OpenParenToken)

-- closeParenParser :: Parser (Located ())
-- closeParenParser = expect $(maybeCaseOf 'CloseParenToken)

-- numberToken :: Parser (Located Integer)
-- numberToken = expect $(maybeCaseOf 'NumberToken)

-- commaParser :: Parser (Located ())
-- commaParser = expect $(maybeCaseOf 'CommaToken)

semiParser :: Parser (Located ())
semiParser = expect $(maybeCaseOf 'SemiToken)

ampersandParser :: Parser (Located ())
ampersandParser = expect $(maybeCaseOf 'AmpersandToken)

-- greaterParser :: Parser (Located ())
-- greaterParser = expect $(maybeCaseOf 'GreaterToken)

-- doubleGreaterParser :: Parser (Located ())
-- doubleGreaterParser = expect $(maybeCaseOf 'DoubleGreaterToken)

barParser :: Parser (Located ())
barParser = expect $(maybeCaseOf 'BarToken)

expect :: (Token -> Maybe a) -> Parser (Located a)
expect f =
  Mega.token
    (\case
       l@(Located {locatedThing = tok})
         | Just token <- f tok -> Just (fmap (const token) l)
       _ -> Nothing)
    mempty

utf8Text :: Located ByteString -> Parser (Located Text)
utf8Text lstr =
  case T.decodeUtf8' (locatedThing lstr) of
    Left _ -> fail ("Invalid UTF-8 string: " ++ show (locatedThing lstr))
    Right v -> pure (fmap (const v) lstr)
