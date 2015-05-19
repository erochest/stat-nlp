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

indexTokens :: (Eq t, Hashable t, Functor f, Foldable f)
            => f (Token p t) -> InverseIndex t p
indexTokens = inverseIndex tokenNorm tokenPos

indexDocumentTokens :: (Eq t, Hashable t, Functor f, Foldable f)
                    => DocumentId -> f (Token p t) -> InverseIndex t (DocumentPos p)
indexDocumentTokens dId ts = (dId,) <$> indexTokens ts

indexDocument :: (Eq t, Hashable t, Functor f, Foldable f)
              => Document (f (Token p t)) -> InverseIndex t (DocumentPos p)
indexDocument (Document did _ dts) = indexDocumentTokens did dts

readIndexDocument :: Corpus p -> Document ()
                  -> IO (InverseIndex PlainToken (DocumentPos p))
readIndexDocument c d = indexDocument <$> tokenizeDocument c d

empty :: IxIndex k
empty = IxIndex M.empty M.empty 0

singleton :: Hashable k => k -> IxIndex k
singleton k = IxIndex (M.singleton k 0) (M.singleton 0 k) 1

null :: IxIndex k -> Bool
null = (== 0) . indexSize

size :: IxIndex k -> Int
size = indexSize

memberItem :: (Eq k, Hashable k) => k -> IxIndex k -> Bool
memberItem k = M.member k . indexItems

memberIx :: Int -> IxIndex k -> Bool
memberIx i = M.member i . indexIxs

insertItem :: (Eq k, Hashable k) => k -> IxIndex k -> IxIndex k
insertItem k i@(IxIndex is ixs s) =
    case M.lookup k is of
        Just _  -> i
        Nothing -> IxIndex (M.insert k s is) (M.insert s k ixs) $ succ s

lookupItem :: (Eq k, Hashable k) => k -> IxIndex k -> Maybe Int
lookupItem k = M.lookup k . indexItems

lookupIx :: Int -> IxIndex k -> Maybe k
lookupIx i = M.lookup i . indexIxs

toList :: IxIndex k -> [k]
toList = M.keys . indexItems

fromList :: (Foldable t, Eq k, Hashable k) => t k -> IxIndex k
fromList = foldl' (flip insertItem) empty

