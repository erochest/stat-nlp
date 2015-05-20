{-# LANGUAGE RecordWildCards #-}


module StatNLP.Corpus
    ( initCorpus
    , makeCorpus
    , addDocument
    , loadCorpusDirectory
    ) where


import           Control.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Text.ICU
import           Data.Traversable
import           System.Directory

import           StatNLP.Document
import qualified StatNLP.Text.Index  as I
import           StatNLP.Text.Tokens
import           StatNLP.Types
import           StatNLP.Utils


initCorpus :: Tokenizer (Token p PlainToken) -> DocumentReader () -> Corpus p
initCorpus t r = Corpus M.empty t r

makeCorpus :: Foldable t
           => Tokenizer (Token p PlainToken) -> DocumentReader () -> t (Document ()) -> Corpus p
makeCorpus tokenizer reader docs =
    Corpus (foldl' insert M.empty docs) tokenizer reader
    where
        insert m d = M.insert (documentKey d) d m

addDocument :: Corpus p -> Document () -> Corpus p
addDocument c d =
    c & corpusDocuments .~ M.insert (documentKey d) d (_corpusDocuments c)

loadCorpusDirectory :: Tokenizer (Token p PlainToken) -> DocumentReader () -> FilePath
                    -> IO (Corpus p)
loadCorpusDirectory tokenizer reader root =
        fmap (makeCorpus tokenizer reader . map initDocument)
    .   walk
    =<< makeAbsolute root
    where
        walk filename = do
            isDir <- doesDirectoryExist filename
            if isDir then walkDirectory filename else return [filename]

indexCorpus :: Corpus p -> IO (IxIndex PlainToken, [Document [Token p Int]])
indexCorpus c = fmap (mapAccumL indexDocumentTokens I.empty)
              . mapM (tokenizeDocument c)
              . M.elems
              $ _corpusDocuments c

indexCorpus' :: Corpus p -> IO (IxIndex PlainToken)
indexCorpus' c = foldlM (fmap (fmap fst) . readIndexDocumentTokens c) I.empty
               . M.elems
               $ _corpusDocuments c
