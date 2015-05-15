{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}


module StatNLP.Types
    ( Corpus(..)
    , Cache
    , DocumentId
    , Document(..)
    , Index(..)
    , PlainToken
    , DocumentPos
    , DocumentLine
    , SpanPos(..)
    , LinePos(..)
    , PlainTokenizer
    , Tokenizer
    , FreqMap
    , Tag
    , Token(..)
    , DocumentReader

    , Context(..)
    , MeasuredContext(..)
    , ContextItem(..)
    , Kwic(..)
    ) where


import           Control.DeepSeq
import qualified Data.FingerTree           as FT
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Sequence             (Seq)
import           Data.String
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Filesystem.Path.CurrentOS
import           GHC.Generics
import           Prelude                   hiding (FilePath)
import           Taygeta.Types             (PlainToken, PlainTokenizer,
                                            Tokenizer)


type FreqMap a      = M.HashMap a Int
type DocumentId     = FilePath
type Tag            = T.Text
type Cache a        = M.HashMap a a
type DocumentReader = Document -> IO T.Text

newtype Index a p = Index { unIndex :: M.HashMap a [p] }

instance Functor (Index a) where
    fmap f (Index m) = Index $ fmap (fmap f) m

instance (Hashable a, Eq a) => Monoid (Index a p) where
    mempty = Index mempty
    mappend (Index a) (Index b) = Index $ M.unionWith mappend a b

data Corpus p = Corpus
              { corpusDocuments :: !(M.HashMap T.Text Document)
              , corpusTokenizer :: !(Tokenizer (Token p))
              , corpusReader    :: !DocumentReader
              }

data Document = Document
              { documentId   :: !DocumentId
              , documentTags :: !(S.HashSet Tag)
              } deriving (Generic)

instance NFData Document

data Token p = Token
             { tokenNorm :: !PlainToken
             , tokenTag  :: !(Maybe Tag)
             , tokenPos  :: !p
             } deriving (Eq, Show, Functor, Generic)

instance Hashable p => Hashable (Token p)

instance NFData p => NFData (Token p)

instance IsString (Token SpanPos) where
    fromString norm = Token (T.pack norm) Nothing . Span 0 $ length norm

type DocumentPos p = (DocumentId, p)
type DocumentLine  = DocumentPos LinePos

data SpanPos = Span { spanStart :: !Int, spanEnd :: !Int }
             deriving (Show, Eq, Generic)

instance Hashable SpanPos

instance NFData SpanPos

data LinePos = Line { posLine :: !Int, posStart :: !Int, posEnd :: !Int }
             deriving (Show, Eq, Ord, Generic)

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

data MeasuredContext a = MContext
                       { mContextSize :: !(Sum Int)
                       , mContextSeq  :: !(FT.FingerTree (Sum Int) (ContextItem a))
                       } deriving (Show, Eq)

newtype ContextItem a = CItem { getContextItem :: a }
                        deriving (Show, Eq)

instance IsString a => IsString (ContextItem a) where
    fromString = CItem . fromString

instance FT.Measured (Sum Int) SpanPos where
    measure (Span start end) = Sum $ end - start

instance FT.Measured (Sum Int) LinePos where
    measure (Line _ start end) = Sum $ end - start

instance FT.Measured (Sum Int) p => FT.Measured (Sum Int) (Token p) where
    measure = FT.measure . tokenPos

instance FT.Measured (Sum Int) a => FT.Measured (Sum Int) (x, a) where
    measure = FT.measure . snd

instance FT.Measured (Sum Int) a => FT.Measured (Sum Int) (ContextItem a) where
    measure = (+ Sum 1) . FT.measure . getContextItem

instance FT.Measured (Sum Int) T.Text where
    measure = Sum . T.length

data Kwic p = Kwic
            { kwicPos    :: !p
            , kwicPrefix :: !T.Text
            , kwicTarget :: !T.Text
            , kwicSuffix :: !T.Text
            } deriving (Show, Eq, Functor)
