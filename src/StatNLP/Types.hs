{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}


module StatNLP.Types
    ( PlainToken
    , PlainTokenizer
    , Tokenizer
    , FreqMap
    , Index(..)
    , Tag
    , Token(..)
    , Line
    , Document
    , LinePos(..)
    , KwicNode(..)
    ) where


import           Conduit
import           Data.Foldable
import qualified Data.HashMap.Strict  as M
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Taygeta.Types        (PlainToken, PlainTokenizer, Tokenizer)


-- TODO: Corpus as a collection of documents, metadata, inverse index
-- TODO: Document as optional source, tags, full text, tokens, other?
-- TODO: Token as norm, tags, and slice into full text (start and end pos)
-- TODO: Norms are cached in order to remove duplicate memory
-- TODO: Line context type that is a comonad/ring buffer
-- TODO: Kwic as line context
-- TODO: concordance as Kwic for all types

type FreqMap a    = M.HashMap a Int
newtype Index a p = Index { unIndex :: M.HashMap a [p] }

type Tag = T.Text

data Token p = Token
             { tokenRaw  :: !PlainToken
             , tokenNorm :: !PlainToken
             , tokenTag  :: !(Maybe Tag)
             , tokenPos  :: !p
             } deriving (Eq, Show, Functor)

type instance Element (Token p) = T.Text

instance MonoFunctor (Token p) where
    omap f token = token { tokenNorm = f (tokenNorm token) }

data LinePos = LinePos
             { posLine :: !Int
             , posCol  :: !Int
             } deriving (Eq, Show)

type Line     = T.Text
type Document = [Line]

-- This tracks a line containing a hit and its immediate n lines of context.
-- If hits occur within n lines of each other, their contexts are put
-- together and returned as a single node.
data KwicNode = KwicNode
