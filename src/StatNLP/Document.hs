{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module StatNLP.Document where


import           Data.Either
import           Data.Hashable
import qualified Data.HashSet              as S
import qualified Data.Text                 as T
import           Data.Text.ICU
import           Data.Traversable
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           StatNLP.Text.Index
import           StatNLP.Text.Tokens
import           StatNLP.Types


initDocument :: DocumentId -> Document ()
initDocument input = Document input S.empty ()

documentKey :: Document ts -> T.Text
documentKey = either id id . toText . documentId

inverseIndexDocumentTokens :: (Eq t, Hashable t, Functor f, Foldable f)
                           => DocumentId -> f (Token p t) -> InverseIndex t (DocumentPos p)
inverseIndexDocumentTokens dId ts = (dId,) <$> indexTokens ts

inverseIndexDocument :: (Eq t, Hashable t, Functor f, Foldable f)
              => Document (f (Token p t)) -> InverseIndex t (DocumentPos p)
inverseIndexDocument (Document did _ dts) = inverseIndexDocumentTokens did dts

readInverseIndexDocument :: Corpus p -> Document ()
                  -> IO (InverseIndex PlainToken (DocumentPos p))
readInverseIndexDocument c d = inverseIndexDocument <$> tokenizeDocument c d

tokenizeDocument :: Corpus p -> Document () -> IO (Document [Token p PlainToken])
tokenizeDocument Corpus{..} d = do
    tokens <- corpusTokenizer <$> corpusReader d
    return $ d { documentTokens = tokens }

indexDocumentTokens :: IxIndex PlainToken -> Document [Token p PlainToken]
                    -> (IxIndex PlainToken, Document [Token p Int])
indexDocumentTokens i d = fmap (setDocumentTokens d)
                        . mapAccumL step i
                        $ documentTokens d
    where
        setDocumentTokens d ts = d { documentTokens = ts }
        setTokenNorm t n = t { tokenNorm = n }
        step index token@Token{tokenNorm} = setTokenNorm token <$> insertItem' tokenNorm index

readIndexDocumentTokens :: Corpus p -> IxIndex PlainToken -> Document ()
                        -> IO (IxIndex PlainToken, Document [Token p Int])
readIndexDocumentTokens c i d = indexDocumentTokens i <$> tokenizeDocument c d
