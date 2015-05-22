{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module StatNLP.Document where


import           Control.Lens
import           Control.Monad
import           Data.BloomFilter.Easy
import qualified Data.BloomFilter.Hash as H
import           Data.Either
import           Data.Foldable         (toList)
import           Data.Hashable
import qualified Data.HashSet          as S
import qualified Data.Text             as T
import           Data.Text.ICU
import           Data.Traversable

import           StatNLP.Text.Index    hiding (toList)
import           StatNLP.Text.Tokens
import           StatNLP.Types


initDocument :: DocumentId -> Document b ()
initDocument input = Document input S.empty Nothing ()

setDocumentTypes :: (Traversable t, H.Hashable n)
                 => Document a ts -> t n -> Document n ts
setDocumentTypes d ts = set documentTypes (Just . easyList 0.01 $ toList ts) d

updateDocumentTypes :: (Traversable t, H.Hashable n)
                    => Document m (t (Token p n)) -> Document n (t (Token p n))
updateDocumentTypes d = setDocumentTypes d $ d ^.. documentTokens . traverse . tokenNorm

readDocumentTypes :: Corpus (Token p PlainToken) p
                  -> Document (Token p PlainToken) ()
                  -> IO (Document (Token p PlainToken) ())
readDocumentTypes c d = void <$> tokenizeDocument c d

readDocumentTypes' :: H.Hashable t
                   => (Document a () -> IO [t]) -> Document a () -> IO (Document t ())
readDocumentTypes' tokenize d = setDocumentTypes d <$> tokenize d

setDocumentTokens :: (Traversable t, H.Hashable n)
                  => Document a b -> (m -> n) -> t m -> Document n (t m)
setDocumentTokens d f ts = setDocumentTypes (d & documentTokens .~ ts) $ fmap f ts

documentKey :: Document b ts -> T.Text
documentKey = T.pack . _documentId

inverseIndexDocumentTokens :: (Eq t, Hashable t, Functor f, Foldable f)
                           => DocumentId -> f (Token p t) -> InverseIndex t (DocumentPos p)
inverseIndexDocumentTokens dId ts = (dId,) <$> indexTokens ts

inverseIndexDocument :: (Eq t, Hashable t, Functor f, Foldable f)
                     => Document b (f (Token p t)) -> InverseIndex t (DocumentPos p)
inverseIndexDocument (Document did _ _ dts) = inverseIndexDocumentTokens did dts

readInverseIndexDocument :: Corpus b p -> Document b ()
                         -> IO (InverseIndex PlainToken (DocumentPos p))
readInverseIndexDocument c d = inverseIndexDocument <$> tokenizeDocument c d

tokenizeDocument :: Corpus b p -> Document b () -> IO (Document (Token p PlainToken) [Token p PlainToken])
tokenizeDocument Corpus{..} d =
    setDocumentTokens d id . _corpusTokenizer <$> _corpusReader d

indexDocumentTokens :: IxIndex PlainToken -> Document b [Token p PlainToken]
                    -> (IxIndex PlainToken, Document Int [Token p Int])
indexDocumentTokens i d = fmap (setDocumentTokens d _tokenNorm)
                        . mapAccumL step i
                        $ _documentTokens d
    where
        step index token@Token{_tokenNorm} = flip (set tokenNorm) token <$> insertItem' _tokenNorm index

readIndexDocumentTokens :: Corpus b p -> IxIndex PlainToken -> Document b ()
                        -> IO (IxIndex PlainToken, Document Int [Token p Int])
readIndexDocumentTokens c i d = indexDocumentTokens i <$> tokenizeDocument c d
