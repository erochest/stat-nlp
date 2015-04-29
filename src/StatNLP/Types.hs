{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}


module StatNLP.Types
    ( Corpus(..)
    , Cache
    , DocumentId
    , Document(..)
    , Index(..)
    , PlainToken
    , DocumentPos
    , SpanPos(..)

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
import qualified Data.HashSet         as S
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Taygeta.Types        (PlainToken, PlainTokenizer, Tokenizer)


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

data Document = Document
              { documentId     :: !DocumentId
              , documentTags   :: !(S.HashSet Tag)
              , documentText   :: !T.Text
              , documentTokens :: ![Token SpanPos]
              }

data Token p = Token
             { tokenNorm :: !PlainToken
             , tokenTag  :: !(Maybe Tag)
             , tokenPos  :: !p
             } deriving (Eq, Show, Functor)

type DocumentPos = (DocumentId, SpanPos)

data SpanPos = Span { spanStart :: !Int, spanEnd :: !Int }
             deriving (Show, Eq)

type instance Element (Token p) = T.Text

instance MonoFunctor (Token p) where
    omap f token = token { tokenNorm = f (tokenNorm token) }

type Line     = T.Text

-- This tracks a line containing a hit and its immediate n lines of context.
-- If hits occur within n lines of each other, their contexts are put
-- together and returned as a single node.
data KwicNode = KwicNode
