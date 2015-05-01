{-# LANGUAGE RecordWildCards #-}


module StatNLP.Corpus
    ( initCorpus
    , addDocument
    , loadCorpusDirectory
    ) where


import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.ICU
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           StatNLP.Document
import           StatNLP.Text.Index
import           StatNLP.Text.Tokens
import           StatNLP.Types
import           StatNLP.Utils


initCorpus :: Corpus p
initCorpus = Corpus M.empty M.empty $ Index M.empty

addDocument :: Corpus p -> Document p -> (Corpus p, Document p)
addDocument Corpus{..} d = (c, d { documentTokens = tokens })
    where
        (cache, tokens) = cacheTokens corpusTokenCache $ documentTokens d
        index = indexDocumentTokens (documentId d) (documentTokens d)
        c = Corpus (M.insert (documentId d) d corpusDocuments)
                   cache
                   $ index <> corpusIndex

addDocument' :: Corpus p -> Document p -> Corpus p
addDocument' c d = fst $ addDocument c d

loadCorpusDirectory :: (FilePath -> IO T.Text) -> Tokenizer (Token p) -> FilePath
                    -> IO (Corpus p)
loadCorpusDirectory reader tokenizer root =
        fmap (foldl' addDocument' initCorpus)
    .   mapM (loadDocument reader tokenizer)
    =<< walk
    =<< canonicalizePath root
    where
        walk filename = do
            isDir <- isDirectory filename
            if isDir then walkDirectory filename else return [filename]
