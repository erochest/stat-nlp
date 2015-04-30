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
    , Context(..)
    , pushRight
    , pushLeft
    , left
    , right
    , trimLeft
    , trimRight
    ) where


import           Conduit
import           Control.Comonad
import           Data.Foldable
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import           Data.MonoTraversable
import           Data.Sequence        (Seq, ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence        as Seq
import qualified Data.Text            as T
import           Taygeta.Types        (PlainToken, PlainTokenizer, Tokenizer)


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

pushLeft :: Context a -> a -> Context a
pushLeft (Context b a ls c rs) x =
    case Seq.viewl rs of
        EmptyL   -> Context b a ls' x rs
        r :< rs' -> Context b a ls' r $ rs' |> x
    where
        ls' = (ls |> c) `trimLeft` b

pushRight :: a -> Context a -> Context a
pushRight x (Context b a ls c rs) =
    case Seq.viewr ls of
        EmptyR   -> Context b a ls x rs'
        ls' :> l -> Context b a (x <| ls') l rs'
    where
        rs' = (c <| rs) `trimRight` a

trimLeft :: Seq a -> Int -> Seq a
trimLeft ss n | Seq.length ss > n = case Seq.viewl ss of
                                        EmptyL   -> ss
                                        _ :< ss' -> trimLeft ss' n
              | otherwise         = ss

trimRight :: Seq a -> Int -> Seq a
trimRight ss n | Seq.length ss > n = case Seq.viewr ss of
                                         EmptyR   -> ss
                                         ss' :> _ -> trimRight ss' n
               | otherwise         = ss

left, right :: Context a -> Maybe (Context a)
left = undefined
right = undefined

{-
 - instance Comonad Context where
 -     extract (Context _ _ _ a _) = a
 -     duplicate c =
 -         c { contextBefore = tail $ iterate left c
 -           , contextView   = c
 -           , contextAfter  = tail $ iterate right c
 -           }
 -}

-- This tracks a line containing a hit and its immediate n lines of context.
-- If hits occur within n lines of each other, their contexts are put
-- together and returned as a single node.
data KwicNode = KwicNode
