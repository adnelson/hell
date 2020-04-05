{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the Hell language.

module Hell.Lexer
  ( lexUnquotedByteString
  , lexQuotedByteString
  , lexQuoted
  , lexUnquoted
  ) where

import           Data.ByteString (ByteString)
import           Data.Char
import           Data.Sequence (Seq(), (<|), (|>))
import           Data.Void
import           Data.Word
import           Hell.Types
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Byte.Lexer as Lexer
import qualified Text.Megaparsec.Byte as Mega
import           Text.Megaparsec.Error

-- | Lex bytes into a series of LTokens.
type Lexer = Mega.Parsec Void ByteString

-- | Open the file strictly and lex it.
lexUnquotedByteString :: FilePath -> ByteString -> Either String (Seq (Located Token))
lexUnquotedByteString fp bs =
  case Mega.runParser (Mega.space *> lexUnquoted <* Mega.eof) fp bs of
    Left e -> Left (errorBundlePretty e)
    Right x -> Right x

-- | Open the file strictly and lex it.
lexQuotedByteString :: FilePath -> ByteString -> Either String (Seq (Located Token))
lexQuotedByteString fp bs =
  case Mega.runParser (Mega.space *> lexQuoted <* Mega.eof) fp bs of
    Left e -> Left (errorBundlePretty e)
    Right x -> Right x

-- | Lex quoted code e.g. @ls -alh@.
lexQuoted :: Lexer (Seq (Located Token))
lexQuoted =
  fmap
    mconcat
    (Mega.some
       (Mega.choice
          [ fmap pure lexComment
          , unquoted
          , substitution
          , var
          , string
          , fmap pure symbol
          , quoted
          ] <*
        Mega.space))
  where
    substitution = do
      begin <- located (SubBeginToken <$ Mega.string "$(")
      inner <- lexQuoted
      end <- located (SubEndToken <$ Mega.string ")")
      pure (begin <| (inner |> end))
    unquoted = do
      begin <- located (SpliceBeginToken <$ Mega.string "${")
      inner <- lexUnquoted
      end <- located (SpliceEndToken <$ Mega.string "}")
      pure (begin <| (inner |> end))
    var =
      fmap
        pure
        (located
           (SpliceVarToken <$>
            (Mega.char (c2w '$') *>
             Mega.takeWhile1P Nothing ((\c -> isAlphaNum c) . w2c))))
    quoted =
      fmap
        pure
        (located
           (QuotedToken <$> Mega.takeWhile1P Nothing (not . boundary . w2c)))
    string = fmap pure lexString
    symbol =
      located
        (Mega.choice
           [ CommaToken <$ Mega.char (c2w ',')
           , SemiToken <$ Mega.char (c2w ';')
           , AmpersandToken <$ Mega.char (c2w '&')
           , DoubleGreaterToken <$ Mega.string ">>"
           , GreaterToken <$ Mega.char (c2w '>')
           , BarToken <$ Mega.char (c2w '|')
           ])
    boundary c =
      c == '"' ||
      c == '$' ||
      c == '}' ||
      c == '#' || c == ' ' || c == ';' || c == ')' || c == '(' || c == '{'

-- | Lex unquoted regular code e.g. @let x = 1@.
lexUnquoted :: Lexer (Seq (Located Token))
lexUnquoted =
  fmap
    mconcat
    (Mega.some
       (Mega.choice
          [ fmap pure lexComment
          , quoted
          , fmap pure lexString
          , fmap pure symbol
          , fmap pure number
          , fmap pure lowerWord
          ] <*
        Mega.space))
  where
    quoted = do
      begin <- located (QuoteBeginToken <$ Mega.string "{")
      Mega.space
      inner <- lexQuoted
      end <- located (QuoteEndToken <$ Mega.string "}")
      Mega.space
      pure (begin <| (inner |> end))
    lowerWord =
      located
        (do c <- Mega.takeWhile1P Nothing (isAlpha . w2c)
            cs <- Mega.takeWhileP Nothing (isAlpha . w2c)
            pure (LowerWordToken (c <> cs)))
    number = located (NumberToken <$> Lexer.decimal)
    symbol =
      located
        (Mega.choice
           [ EqualsToken <$ Mega.char (c2w '=')
           , OpenBracketToken <$ Mega.char (c2w '[')
           , CloseBracketToken <$ Mega.char (c2w ']')
           , OpenParenToken <$ Mega.char (c2w '(')
           , CloseParenToken <$ Mega.char (c2w ')')
           , CommaToken <$ Mega.char (c2w ',')
           ])

lexComment :: Lexer (Located Token)
lexComment =
  located
    (CommentToken <$>
     (Mega.char (c2w '#') *>
      Mega.takeWhileP Nothing (not . (\c -> c == '\r' || c == '\n') . w2c)))

lexString :: Lexer (Located Token)
lexString =
  located
    (StringLiteralToken <$>
     (Mega.char (c2w '"') *> innerString <* Mega.char (c2w '"')))
  where
    innerString = Mega.takeWhileP Nothing (/= c2w '"')

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral

located :: Mega.MonadParsec e s m => m Token -> m (Located Token)
located m = do
  start <- Mega.getSourcePos
  v <- m
  end <- Mega.getSourcePos
  pure (Located start end v)
