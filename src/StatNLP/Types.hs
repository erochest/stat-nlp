{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}


module StatNLP.Types
    ( Corpus(..)
    , Cache
    , DocumentId
    , Document
    , Index(..)
    , PlainToken
    , LinePos(..)
    , DocumentPos

    , PlainTokenizer
    , Tokenizer
    , FreqMap
    , Tag
    , Token(..)
    , Line
    , KwicNode(..)
    ) where


import           Conduit
import           Data.Foldable
import qualified Data.HashMap.Strict  as M
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Taygeta.Types        (PlainToken, PlainTokenizer, Tokenizer)


-- TODO: Document as optional source, tags, full text, tokens, other?
-- TODO: Token as norm, tags, and slice into full text (start and end pos)
-- TODO: Norms are cached in order to remove duplicate memory
-- TODO: Line context type that is a comonad/ring buffer
-- TODO: Kwic as line context
-- TODO: concordance as Kwic for all types

type FreqMap a  = M.HashMap a Int
type DocumentId = T.Text
type Tag        = T.Text
type Cache a    = M.HashMap a a

newtype Index a p = Index { unIndex :: M.HashMap a [p] }

data Corpus = Corpus
            { corpusDocuments  :: !(M.HashMap DocumentId Document)
            , corpusTokenCache :: !(Cache PlainToken)
            , corpusIndex      :: !(Index PlainToken DocumentPos)
            }

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

type DocumentPos = (DocumentId, LinePos)

type Line     = T.Text
type Document = [Line]

-- This tracks a line containing a hit and its immediate n lines of context.
-- If hits occur within n lines of each other, their contexts are put
-- together and returned as a single node.
data KwicNode = KwicNode
