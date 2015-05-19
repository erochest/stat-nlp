{-# LANGUAGE RecordWildCards #-}


module StatNLP.Corpus
    ( initCorpus
    , makeCorpus
    , addDocument
    , loadCorpusDirectory
    , documentTokens
    ) where


import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.ICU
import qualified Data.Vector               as V
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           StatNLP.Document
import           StatNLP.Text.Tokens
import           StatNLP.Types
import           StatNLP.Utils


initCorpus :: Tokenizer (Token p PlainToken) -> DocumentReader -> Corpus p
initCorpus t r = Corpus M.empty t r

makeCorpus :: Foldable t
           => Tokenizer (Token p PlainToken) -> DocumentReader -> t Document -> Corpus p
makeCorpus tokenizer reader docs =
    Corpus (foldl' insert M.empty docs) tokenizer reader
    where
        insert m d = M.insert (documentKey d) d m

addDocument :: Corpus p -> Document -> Corpus p
addDocument c d =
    c { corpusDocuments = M.insert (documentKey d) d (corpusDocuments c) }

loadCorpusDirectory :: Tokenizer (Token p PlainToken) -> DocumentReader -> FilePath
                    -> IO (Corpus p)
loadCorpusDirectory tokenizer reader root =
        fmap (makeCorpus tokenizer reader . map initDocument)
    .   walk
    =<< canonicalizePath root
    where
        walk filename = do
            isDir <- isDirectory filename
            if isDir then walkDirectory filename else return [filename]

documentTokens :: Corpus p -> Document -> IO [Token p PlainToken]
documentTokens Corpus{..} d = corpusTokenizer <$> corpusReader d
