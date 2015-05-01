{-# LANGUAGE TupleSections #-}


module StatNLP.Text.Index where


import           Control.Arrow
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.Foldable

import           StatNLP.Types


inverseIndex :: (Functor f, Foldable f, Hashable k, Eq k)
             => (a -> k) -> (a -> p) -> f a -> Index k p
inverseIndex key pos = Index . toMapWith (++) . fmap (key &&& (pure . pos))
    where
        toMapWith f = foldl' (insert f) M.empty
        insert f m (k, v) = M.insertWith f k v m

indexTokens :: (Functor f, Foldable f)
            => f (Token SpanPos) -> Index PlainToken SpanPos
indexTokens = inverseIndex tokenNorm tokenPos

indexDocumentTokens :: (Functor f, Foldable f)
                    => DocumentId -> f (Token SpanPos)
                    -> Index PlainToken DocumentPos
indexDocumentTokens dId ts = (dId,) <$> indexTokens ts
