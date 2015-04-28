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
    , LinePos(..)
    ) where


import           Conduit
import           Data.Foldable
import qualified Data.HashMap.Strict  as M
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Taygeta.Types        (PlainToken, PlainTokenizer, Tokenizer)


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
