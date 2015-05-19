{-# LANGUAGE TupleSections #-}


module StatNLP.Text.Index where


import           Control.Arrow
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.Foldable

import           StatNLP.Corpus
import           StatNLP.Types


inverseIndex :: (Functor f, Foldable f, Hashable k, Eq k)
             => (a -> k) -> (a -> p) -> f a -> InverseIndex k p
inverseIndex key pos = InverseIndex . toMapWith (++) . fmap (key &&& (pure . pos))
    where
        toMapWith f = foldl' (insert f) M.empty
        insert f m (k, v) = M.insertWith f k v m

indexTokens :: (Eq t, Hashable t, Functor f, Foldable f) => f (Token p t) -> InverseIndex t p
indexTokens = inverseIndex tokenNorm tokenPos

indexDocumentTokens :: (Eq t, Hashable t, Functor f, Foldable f)
                    => DocumentId -> f (Token p t) -> InverseIndex t (DocumentPos p)
indexDocumentTokens dId ts = (dId,) <$> indexTokens ts

readIndexDocument :: Corpus p -> Document
                  -> IO (InverseIndex PlainToken (DocumentPos p))
readIndexDocument c d =
    indexDocumentTokens (documentId d) <$> documentTokens c d
