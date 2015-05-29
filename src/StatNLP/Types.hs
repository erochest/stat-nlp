{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}


module StatNLP.Types
    ( Corpus(..)
    , corpusDocuments
    , corpusTokenizer
    , corpusReader

    , Cache
    , DocumentId

    , Document(..)
    , documentId
    , documentTags
    , documentTypes
    , documentTokens

    , IxIndex(..)
    , indexItems
    , indexIxs
    , indexSize

    , InverseIndex(..)
    , MonoidHash(..)
    , PlainToken
    , DocumentPos
    , DocumentLine
    , StopWords
    , VectorDoc
    , LineToken

    , SpanPos(..)
    , spanStart
    , spanEnd

    , LinePos(..)
    , posLine
    , posStart
    , posEnd

    , PlainTokenizer
    , Tokenizer
    , FreqMap
    , Tag

    , Token(..)
    , tokenNorm
    , tokenTag
    , tokenPos

    , DocumentReader
    , DocumentTransformer

    , Context(..)
    , contextBeforeN
    , contextAfterN
    , contextBefore
    , contextView
    , contextAfter

    , MeasuredContext(..)
    , mContextSize
    , mContextSeq

    , ContextItem(..)

    , Kwic(..)
    , kwicPos
    , kwicPrefix
    , kwicTarget
    , kwicSuffix

    , Collocate(..)
    , colHead
    , colPair
    , colDist
    ) where


import           Control.DeepSeq
import           Control.Lens          hiding (Context)
import           Data.BloomFilter      (Bloom)
import qualified Data.BloomFilter.Hash as H
import qualified Data.FingerTree       as FT
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict   as M
import qualified Data.HashSet          as S
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Sequence         (Seq)
import           Data.String
import qualified Data.Text             as T
import           Data.Text.Encoding    (encodeUtf8)
import qualified Data.Vector           as V
import           GHC.Exts
import           GHC.Generics
import           Taygeta.Types         (PlainToken, PlainTokenizer, Tokenizer)


newtype MonoidHash a p = MHash { unHash :: M.HashMap a p }

instance Functor (MonoidHash a) where
    fmap f (MHash m) = MHash $ fmap f m

instance (Hashable k, Eq k, Monoid v) => Monoid (MonoidHash k v) where
    mempty = MHash mempty
    mappend (MHash a) (MHash b) = MHash $ M.unionWith mappend a b

type FreqMap a  = MonoidHash a (Sum Int)
type DocumentId = FilePath
type Tag        = T.Text
type Cache a    = M.HashMap a a

data Document b ts = Document
                   { _documentId     :: !DocumentId
                   , _documentTags   :: !(S.HashSet Tag)
                   , _documentTypes  :: !(Maybe (Bloom b))
                   , _documentTokens :: !ts
                   } deriving (Generic, Functor, Foldable, Traversable)
makeLenses ''Document

instance (NFData b, NFData ts) => NFData (Document b ts)

type DocumentReader b ts = Document b ts -> IO T.Text

type DocumentTransformer b ts = Document b ts -> IO (Document b ts)

data IxIndex a = IxIndex
               { _indexItems :: !(M.HashMap a Int)
               , _indexIxs   :: !(M.HashMap Int a)
               , _indexSize  :: !Int
               } deriving (Show, Eq)
makeLenses ''IxIndex

instance (Eq a, Hashable a) => IsList (IxIndex a) where
    type Item (IxIndex a) = a
    fromList = foldl' insert (IxIndex M.empty M.empty 0)
        where
            insert i@(IxIndex is ixs s) k =
                case M.lookup k is of
                    Just _  -> i
                    Nothing -> IxIndex (M.insert k s is) (M.insert s k ixs) $ succ s
    toList   = M.keys . _indexItems

newtype InverseIndex a p = InverseIndex { unIndex :: M.HashMap a [p] }

instance Functor (InverseIndex a) where
    fmap f (InverseIndex m) = InverseIndex $ fmap (fmap f) m

instance (Hashable a, Eq a) => Monoid (InverseIndex a p) where
    mempty = InverseIndex mempty
    mappend (InverseIndex a) (InverseIndex b) =
        InverseIndex $ M.unionWith mappend a b

data SpanPos = Span { _spanStart :: !Int, _spanEnd :: !Int }
             deriving (Show, Eq, Generic)
makeLenses ''SpanPos

instance Hashable SpanPos

instance NFData SpanPos

data LinePos = Line { _posLine :: !Int, _posStart :: !Int, _posEnd :: !Int }
             deriving (Show, Eq, Ord, Generic)
makeLenses ''LinePos

instance Hashable LinePos

instance NFData LinePos

data Token p t = Token
               { _tokenNorm :: !t
               , _tokenTag  :: !(Maybe Tag)
               , _tokenPos  :: !p
               } deriving (Eq, Show, Functor, Generic)
makeLenses ''Token

instance (Hashable t, Hashable p) => Hashable (Token p t)

instance (NFData t, NFData p) => NFData (Token p t)

instance IsString (Token SpanPos PlainToken) where
    fromString norm = Token (T.pack norm) Nothing . Span 0 $ length norm

type instance Element (Token p t) = T.Text

instance H.Hashable (Token p PlainToken) where
    hashIO32 (Token pt _ _) = H.hashIO32 (encodeUtf8 pt)
    hashIO64 (Token pt _ _) = H.hashIO64 (encodeUtf8 pt)

data Corpus b p = Corpus
                { _corpusDocuments :: !(M.HashMap T.Text (Document b ()))
                , _corpusTokenizer :: !(Tokenizer (Token p PlainToken))
                , _corpusReader    :: !(DocumentReader b ())
                }
makeLenses ''Corpus

type DocumentPos p = (DocumentId, p)
type DocumentLine  = DocumentPos LinePos
type StopWords     = S.HashSet PlainToken
type VectorDoc b   = Document b (V.Vector (Token Int PlainToken))
type LineToken     = Token LinePos PlainToken

data Context a = Context
               { _contextBeforeN :: !Int
               , _contextAfterN  :: !Int
               , _contextBefore  :: !(Seq a)
               , _contextView    :: !a
               , _contextAfter   :: !(Seq a)
               } deriving (Show, Eq)
makeLenses ''Context

instance Functor Context where
    fmap f (Context b a before current after) =
        Context b a (fmap f before) (f current) (fmap f after)

newtype ContextItem a = CItem { getContextItem :: a }
                        deriving (Show, Eq)

instance IsString a => IsString (ContextItem a) where
    fromString = CItem . fromString

data MeasuredContext a = MContext
                       { _mContextSize :: !(Sum Int)
                       , _mContextSeq  :: !(FT.FingerTree (Sum Int) (ContextItem a))
                       } deriving (Show, Eq)
makeLenses ''MeasuredContext

instance FT.Measured (Sum Int) SpanPos where
    measure (Span start end) = Sum $ end - start

instance FT.Measured (Sum Int) LinePos where
    measure (Line _ start end) = Sum $ end - start

instance FT.Measured (Sum Int) p => FT.Measured (Sum Int) (Token p t) where
    measure = FT.measure . _tokenPos

instance FT.Measured (Sum Int) a => FT.Measured (Sum Int) (x, a) where
    measure = FT.measure . snd

instance FT.Measured (Sum Int) a => FT.Measured (Sum Int) (ContextItem a) where
    measure = (+ Sum 1) . FT.measure . getContextItem

instance FT.Measured (Sum Int) T.Text where
    measure = Sum . T.length

data Kwic p = Kwic
            { _kwicPos    :: !p
            , _kwicPrefix :: !T.Text
            , _kwicTarget :: !T.Text
            , _kwicSuffix :: !T.Text
            } deriving (Show, Eq, Functor)
makeLenses ''Kwic

data Collocate a = Collocate
                 { _colHead :: !a
                 , _colPair :: !a
                 , _colDist :: !Int
                 } deriving (Show, Eq, Ord, Functor)
makeLenses ''Collocate
