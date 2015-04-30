{-# LANGUAGE RecordWildCards #-}


module StatNLP.Corpus
    ( initCorpus
    , addDocument
    ) where


import           Data.Monoid
import qualified Data.HashMap.Strict as M

import           StatNLP.Text.Index
import           StatNLP.Text.Tokens
import           StatNLP.Types


initCorpus :: Corpus
initCorpus = Corpus M.empty M.empty $ Index M.empty

addDocument :: Corpus -> Document -> (Corpus, Document)
addDocument Corpus{..} d = (c, d { documentTokens = tokens })
    where
        (cache, tokens) = cacheTokens corpusTokenCache $ documentTokens d
        index = indexDocumentTokens (documentId d) (documentTokens d)
        c = Corpus (M.insert (documentId d) d corpusDocuments)
                   cache
                   $ index <> corpusIndex
