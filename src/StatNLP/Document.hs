{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module StatNLP.Document where


import           Data.Either
import           Data.Hashable
import qualified Data.HashSet              as S
import qualified Data.Text                 as T
import           Data.Text.ICU
import qualified Data.Vector               as V
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

indexDocumentTokens :: (Eq t, Hashable t, Functor f, Foldable f)
                    => DocumentId -> f (Token p t) -> InverseIndex t (DocumentPos p)
indexDocumentTokens dId ts = (dId,) <$> indexTokens ts

indexDocument :: (Eq t, Hashable t, Functor f, Foldable f)
              => Document (f (Token p t)) -> InverseIndex t (DocumentPos p)
indexDocument (Document did _ dts) = indexDocumentTokens did dts

readIndexDocument :: Corpus p -> Document ()
                  -> IO (InverseIndex PlainToken (DocumentPos p))
readIndexDocument c d = indexDocument <$> tokenizeDocument c d

tokenizeDocument :: Corpus p -> Document () -> IO (Document [Token p PlainToken])
tokenizeDocument Corpus{..} d = do
    tokens <- corpusTokenizer <$> corpusReader d
    return $ d { documentTokens = tokens }
