{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TypeFamilies          #-}


module StatNLP.Types
    ( Corpus(..)
    , Cache
    , DocumentId
    , Document(..)
    , IxIndex(..)
    , InverseIndex(..)
    , MonoidHash(..)
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
import           GHC.Exts
import           GHC.Generics
import           Prelude                   hiding (FilePath)
import           Taygeta.Types             (PlainToken, PlainTokenizer,
                                            Tokenizer)


type FreqMap a         = MonoidHash a (Sum Int)
type DocumentId        = FilePath
type Tag               = T.Text
type Cache a           = M.HashMap a a
type DocumentReader ts = Document ts -> IO T.Text

data IxIndex a = IxIndex
               { indexItems :: !(M.HashMap a Int)
               , indexIxs   :: !(M.HashMap Int a)
               , indexSize  :: !Int
               } deriving (Show, Eq)


instance (Eq a, Hashable a) => IsList (IxIndex a) where
    type Item (IxIndex a) = a
    fromList = foldl' insert (IxIndex M.empty M.empty 0)
        where
            insert i@(IxIndex is ixs s) k =
                case M.lookup k is of
                    Just _  -> i
                    Nothing -> IxIndex (M.insert k s is) (M.insert s k ixs) $ succ s
    toList   = M.keys . indexItems

newtype InverseIndex a p = InverseIndex { unIndex :: M.HashMap a [p] }

instance Functor (InverseIndex a) where
    fmap f (InverseIndex m) = InverseIndex $ fmap (fmap f) m

instance (Hashable a, Eq a) => Monoid (InverseIndex a p) where
    mempty = InverseIndex mempty
    mappend (InverseIndex a) (InverseIndex b) =
        InverseIndex $ M.unionWith mappend a b

newtype MonoidHash a p = MHash { unHash :: M.HashMap a p }

instance Functor (MonoidHash a) where
    fmap f (MHash m) = MHash $ fmap f m

instance (Hashable k, Eq k, Monoid v) => Monoid (MonoidHash k v) where
    mempty = MHash mempty
    mappend (MHash a) (MHash b) = MHash $ M.unionWith mappend a b

data Corpus p = Corpus
              { corpusDocuments :: !(M.HashMap T.Text (Document ()))
              , corpusTokenizer :: !(Tokenizer (Token p PlainToken))
              , corpusReader    :: !(DocumentReader ())
              }

data Document ts = Document
                 { documentId     :: !DocumentId
                 , documentTags   :: !(S.HashSet Tag)
                 , documentTokens :: !ts
                 } deriving (Generic, Functor)

instance NFData ts => NFData (Document ts)

data Token p t = Token
               { tokenNorm :: !t
               , tokenTag  :: !(Maybe Tag)
               , tokenPos  :: !p
               } deriving (Eq, Show, Functor, Generic)

instance (Hashable t, Hashable p) => Hashable (Token p t)

instance (NFData t, NFData p) => NFData (Token p t)

instance IsString (Token SpanPos PlainToken) where
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

type instance Element (Token p t) = T.Text

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

instance FT.Measured (Sum Int) p => FT.Measured (Sum Int) (Token p t) where
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
