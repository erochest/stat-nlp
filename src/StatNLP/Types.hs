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
    , LinePos(..)
    , PlainTokenizer
    , Tokenizer
    , FreqMap
    , Tag
    , Token(..)
    , DocumentReader

    , Context(..)
    ) where


import           Control.DeepSeq
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.MonoTraversable
import           Data.Sequence             (Seq)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Filesystem.Path.CurrentOS
import           GHC.Generics
import           Prelude                   hiding (FilePath)
import           Taygeta.Types             (PlainToken, PlainTokenizer, Tokenizer)


-- TODO: Kwic as line context
-- TODO: concordance as Kwic for all types

type FreqMap a      = M.HashMap a Int
type DocumentId     = FilePath
type Tag            = T.Text
type Cache a        = M.HashMap a a
type DocumentReader = Document -> IO [T.Text]

newtype Index a p = Index { unIndex :: M.HashMap a [p] }

instance Functor (Index a) where
    fmap f (Index m) = Index $ fmap (fmap f) m

instance (Hashable a, Eq a) => Monoid (Index a p) where
    mempty = Index mempty
    mappend (Index a) (Index b) = Index $ M.unionWith mappend a b

data Corpus p = Corpus
              { corpusDocuments  :: !(M.HashMap T.Text Document)
              , corpusTokenizer  :: !(Tokenizer (Token p))
              , corpusReader     :: !DocumentReader
              }

data Document = Document
              { documentId     :: !DocumentId
              , documentTags   :: !(S.HashSet Tag)
              } deriving (Generic)

instance NFData Document

data Token p = Token
             { tokenNorm :: !PlainToken
             , tokenTag  :: !(Maybe Tag)
             , tokenPos  :: !p
             } deriving (Eq, Show, Functor, Generic)

instance Hashable p => Hashable (Token p)

instance NFData p => NFData (Token p)

type DocumentPos p = (DocumentId, p)

data SpanPos = Span { spanStart :: !Int, spanEnd :: !Int }
             deriving (Show, Eq, Generic)

instance Hashable SpanPos

instance NFData SpanPos

data LinePos = Line { posLine :: !Int, posStart :: !Int, posEnd :: !Int }
             deriving (Show, Eq, Generic)

instance Hashable LinePos

instance NFData LinePos

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
