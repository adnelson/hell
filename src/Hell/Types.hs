{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | All types for the language.

module Hell.Types where

import           Control.Concurrent (ThreadId)
import           Data.ByteString (ByteString)
import           Data.Conduit (ConduitT)
import           Data.Data
import           Data.Foldable
import           Data.Monoid
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Prelude hiding (error)
import           System.Exit (ExitCode)
import qualified Text.Megaparsec as Mega
import           GHC.Base (NonEmpty)

-- | A shell action.
data Action r where
  Command :: Text -> [Text] -> Action ExitCode
  -- ^ A command reads/writes and returns an exit code.

  Pipe :: Action _a -> Action r -> Action r
  -- ^ Piped commands certainly read/write to eachother, but no
  -- constraints other than that.

  Sequence :: Action _a -> Action r -> Action r
  -- ^ A sequence of commands doesn't necessarily write anything.

  Redirect :: Action r -> Redirect -> Action r
  -- ^ A redirect may read, must accept a 'Action' which writes, but
  -- produces '()' output in its final type, instead writing the
  -- output to a file.

  Background :: Action r -> Action ThreadId
  -- ^ Launch a thread process in the background. Returns the thread
  -- id, produces no pipeable output.

  Substitution :: Action _o -> (ByteString -> Action r) -> Action r
  -- ^ Run a shell producing an output, consuming no input, and return
  -- value unused. Feed its output into the continuation.

  Conduit :: ConduitT ByteString ByteString IO () -> Action ()
  -- ^ Run a pure Haskell conduit.

  ChangeDirectory :: DirectoryChange -> Action ExitCode
  -- ^ Change default directory of the current process.

-- | A directory change.
data DirectoryChange
  = GoHome
  | Chdir FilePath

data SomeAction =
  forall r. SomeAction (Action r)

-- | Redirection of a process output.
data Redirect
  = StdoutTo To
  | StderrTo To

-- | Redirect process output to.
data To
  = ToStdout
  | ToStderr
  | ToFile FilePath
  | ToFileAppend FilePath

-- | A located token.
data Located l = Located
  { locatedStart :: !Mega.SourcePos
  , locatedEnd :: !Mega.SourcePos
  , locatedThing :: !l
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Lexical tokens for the Hell language.
data Token
  = SpliceBeginToken
  | SpliceEndToken
  | SpliceVarToken !ByteString
  | QuoteBeginToken
  | QuoteEndToken
  | QuotedToken !ByteString
  | SubBeginToken
  | SubEndToken
  | StringLiteralToken !ByteString
  | CommentToken !ByteString
  | LowerWordToken !ByteString
  | EqualsToken
  | OpenBracketToken
  | CloseBracketToken
  | OpenParenToken
  | CloseParenToken
  | NumberToken !Integer
  | CommaToken
  | SemiToken
  | AmpersandToken
  | GreaterToken
  | DoubleGreaterToken
  | BarToken
  deriving (Show, Eq, Ord, Data)

-- | Alias for disambiguation in Stream instance
type Token' = Token

showToken :: Token -> String
showToken =
  \case
    SpliceBeginToken -> quote "open splice" "${"
    SpliceEndToken -> quote "splice end" "}"
    SpliceVarToken !sv ->
      quote "variable splice" ("$" <> T.unpack (T.decodeUtf8 sv))
    QuoteBeginToken -> quote "begin quote" "{"
    QuoteEndToken -> quote "end quote" "}"
    QuotedToken !q -> quote "quoted content " (T.unpack (T.decodeUtf8 q))
    SubBeginToken -> quote "begin substitution" "$("
    SubEndToken -> quote "end substitution" ")"
    StringLiteralToken !s -> quote "string literal" (show s)
    CommentToken !c -> quote "comment" ("#" <> T.unpack (T.decodeUtf8 c))
    LowerWordToken !w -> quote "word" (T.unpack (T.decodeUtf8 w))
    EqualsToken -> quote "equals" "="
    OpenBracketToken -> quote "open bracket" "["
    CloseBracketToken -> quote "closing bracket" "]"
    OpenParenToken -> quote "opening paren" "("
    CloseParenToken -> quote "closing paren" ")"
    NumberToken !i -> quote "number" (show i)
    CommaToken -> quote "comma" ","
    SemiToken -> quote "semicolon" ";"
    AmpersandToken -> quote "ampersand" "&"
    GreaterToken -> quote "greater than" ">"
    DoubleGreaterToken -> quote "double greater than" ">>"
    BarToken -> quote "pipe" "|"
  where
    quote label thing = label ++ " ‘" ++ thing ++ "’"

-- | A sequence of located tokens
newtype LTokens = LTokens (Seq (Located Token)) deriving (Show, Eq, Ord)

-- | This instance gives support to parse LTokens with megaparsec.
instance Mega.Stream LTokens where
  type Token LTokens = Located Token'
  type Tokens LTokens = Seq (Located Token')
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = Seq.fromList
  chunkToTokens Proxy = toList
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (LTokens Seq.Empty) = Nothing
  take1_ (LTokens (t :<| ts)) = Just (t, LTokens ts)
  takeN_ n (LTokens s)
    | n <= 0   = Just (mempty, LTokens s)
    | null s   = Nothing
    | otherwise = let (s', s'') = Seq.splitAt n s in Just (s', LTokens s'')
  takeWhile_ cond (LTokens tokens) = let (ts, ts') = Seq.spanl cond tokens in (ts, LTokens ts')
  showTokens Proxy = unwords . map (showToken . locatedThing) . toList

  -- TODO
  -- reachOffset offset initialPosState
