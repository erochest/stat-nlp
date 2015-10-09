{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module StatNLP.Types
    ( Corpus(..)
    , corpusDocuments
    , corpusTokenizer
    , corpusReader

    , Cache
    , DocumentId

    , Frequencies(..)
    , ProbMap
    , ConditionalProbMap
    , ProbabilityDist(..)
    , Probabilistic(..)

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
    , ConditionalFreq
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

    , SummaryStats(..)
    , summaryN
    , summaryMean
    , summaryVariance
    ) where


import           Control.Arrow                    ((&&&))
import           Control.DeepSeq
import           Control.Lens                     hiding (Context)
import           Control.Monad.Primitive
import           Data.BloomFilter                 (Bloom)
import qualified Data.BloomFilter.Hash            as H
import qualified Data.FingerTree                  as FT
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict              as M
import qualified Data.HashSet                     as S
import qualified Data.List                        as L
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Ord
import           Data.Sequence                    (Seq)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as GV
import           GHC.Exts
import           GHC.Generics
import           GHC.Word                         (Word32)
import           System.Random.MWC
import           System.Random.MWC.CondensedTable
import           Taygeta.Types                    (PlainToken, PlainTokenizer,
                                                   Tokenizer)


newtype MonoidHash a p = MHash { unHash :: M.HashMap a p }
                         deriving (NFData)

instance Functor (MonoidHash a) where
    fmap f (MHash m) = MHash $ fmap f m

instance (Hashable k, Eq k, Monoid v) => Monoid (MonoidHash k v) where
    mempty = MHash mempty
    mappend (MHash a) (MHash b) = MHash $ M.unionWith mappend a b

type FreqMap a              = MonoidHash a (Sum Int)
type ConditionalFreq a b    = MonoidHash a (FreqMap b)
type ProbMap a              = M.HashMap a Double
type ConditionalProbMap a b = M.HashMap a (ProbMap b)
type DocumentId             = FilePath
type Tag                    = T.Text
type Cache a                = M.HashMap a a

class Probabilistic d s where
        -- | Return the probability [0.0-1.0] for @o@.
        probability :: d -> s -> Double

        -- | Return the log probability, if @probability@ doesn't return 0.
        logProbability :: d -> s -> Maybe Double

        logProbability d o = let p = probability d o
                             in  if p == 0 then Nothing else Just (logBase p 2)

class Probabilistic d s => ProbabilityDist d s where
        -- | Return the sample with the maximum probability.
        maxProbability :: d -> Maybe s

        -- | Return all samples with >0 probability.
        samples :: d -> [s]

        -- | Return the ratio by which counts are discounted on average.
        discount :: d -> Double

        -- | Return a randomly selected item from this distribution.
        generate :: PrimMonad m => d -> Gen (PrimState m) -> m s

        -- | Create a condensed look-up table for this distribution.
        table :: ( GV.Vector v (s, Double)
                 , GV.Vector v (s, Word32)
                 , GV.Vector v s
                 , GV.Vector v Double
                 , GV.Vector v Word32)
              => d -> CondensedTable v s

        maxProbability d =
            case samples d of
                [] -> Nothing
                xs -> Just
                   .  fst
                   .  L.maximumBy (comparing snd)
                   $  fmap (id &&& probability d) xs

        discount _ = 0.0

        table d = tableFromProbabilities
                . GV.fromList
                . fmap (id &&& probability d)
                $ samples d

        generate d = genFromTable t
            where
                t :: CondensedTableV s
                t = table d

class Monoid f => Frequencies f where
    type FItem f

    count :: f -> FItem f -> f
    countAll :: Traversable t => f -> t (FItem f) -> f
    frequency :: f -> FItem f -> Int
    frequencyItems :: f -> [FItem f]
    total :: f -> Int

    countAll = foldl' count
    total f = sum . map (frequency f) $ frequencyItems f

instance (Eq a, Hashable a) => Frequencies (FreqMap a) where
    type FItem (FreqMap a) = a

    count fqs x = MHash . M.insertWith mappend x 1 $ unHash fqs
    frequency fqs x = getSum . M.lookupDefault 0 x $ unHash fqs
    frequencyItems = M.keys . unHash
    total = getSum . mconcat . M.elems . unHash

instance (Eq a, Hashable a, Eq b, Hashable b)
    => Frequencies (ConditionalFreq a b) where

    type FItem (ConditionalFreq a b) = (a, b)

    count fqs (a, b) =
        MHash . M.insertWith mappend a (MHash $ M.singleton b 1) $ unHash fqs
    countAll f xs =
        let step m (a, b) = M.insertWith mappend a (MHash $ M.singleton b 1) m
        in  MHash . flip (foldl' step) xs $ unHash f
    frequency (MHash f) (a, b) =
        getSum . fold $ M.lookup b . unHash =<< M.lookup a f
    frequencyItems =
        concatMap (sequenceA . fmap (M.keys . unHash)) . M.toList . unHash
    total = getSum . foldMap (mconcat . M.elems . unHash) . M.elems . unHash

data Document b ts = Document
                   { _documentId     :: !DocumentId
                   , _documentTags   :: !(S.HashSet Tag)
                   , _documentTypes  :: !(Maybe (Bloom b))
                   , _documentTokens :: !ts
                   } deriving (Show, Generic, Functor, Foldable, Traversable)
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
                    Nothing -> IxIndex (M.insert k s is) (M.insert s k ixs)
                               $ succ s
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

data MeasuredContext a
    = MContext
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

data SummaryStats = SummaryStats
                  { _summaryN        :: !Int
                  , _summaryMean     :: !Double
                  , _summaryVariance :: !Double
                  } deriving (Show, Eq)
makeLenses ''SummaryStats
