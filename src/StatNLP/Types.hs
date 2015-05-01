{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

    , Context(..)
    ) where


import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import           Data.MonoTraversable
import           Data.Sequence        (Seq)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           GHC.Generics
import           Taygeta.Types        (PlainToken, PlainTokenizer, Tokenizer)


-- TODO: Kwic as line context
-- TODO: concordance as Kwic for all types

type FreqMap a  = M.HashMap a Int
type DocumentId = T.Text
type Tag        = T.Text
type Cache a    = M.HashMap a a

newtype Index a p = Index { unIndex :: M.HashMap a [p] }

instance Functor (Index a) where
    fmap f (Index m) = Index $ fmap (fmap f) m

instance (Hashable a, Eq a) => Monoid (Index a p) where
    mempty = Index mempty
    mappend (Index a) (Index b) = Index $ M.unionWith mappend a b

data Corpus = Corpus
            { corpusDocuments  :: !(M.HashMap DocumentId Document)
            , corpusTokenCache :: !(Cache PlainToken)
            , corpusIndex      :: !(Index PlainToken DocumentPos)
            }

data Document = Document
              { documentId     :: !DocumentId
              , documentTags   :: !(S.HashSet Tag)
              , documentText   :: !T.Text
              , documentTokens :: !(V.Vector (Token SpanPos))
              }

data Token p = Token
             { tokenNorm :: !PlainToken
             , tokenTag  :: !(Maybe Tag)
             , tokenPos  :: !p
             } deriving (Eq, Show, Functor, Generic)

instance Hashable p => Hashable (Token p)

type DocumentPos = (DocumentId, SpanPos)

data SpanPos = Span { spanStart :: !Int, spanEnd :: !Int }
             deriving (Show, Eq, Generic)

instance Hashable SpanPos

type instance Element (Token p) = T.Text

instance MonoFunctor (Token p) where
    omap f token = token { tokenNorm = f (tokenNorm token) }

data Context a = Context
               { contextBeforeN :: !Int
               , contextAfterN  :: !Int
               , contextBefore  :: !(Seq a)
               , contextView    :: !a
               , contextAfter   :: !(Seq a)
               } deriving (Show, Eq)

instance Functor Context where
    fmap f (Context b a before current after) =
        Context b a (fmap f before) (f current) (fmap f after)
