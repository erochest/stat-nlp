

module StatNLP.Corpus
    ( initCorpus
    , makeCorpus
    , addDocument
    , updateDocumentFilters
    , loadCorpusDirectory
    , indexCorpus
    , indexCorpus'
    , inverseIndexCorpus
    , readCorpusVectors
    , transformer
    ) where


import           Control.Arrow       ((&&&))
import           Control.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Text.ICU
import           Data.Traversable
import qualified Data.Vector         as V
import           System.Directory

import           StatNLP.Document
import           StatNLP.Input
import           StatNLP.Parallel
import qualified StatNLP.Text.Index  as I
import           StatNLP.Text.Tokens
import           StatNLP.Types
import           StatNLP.Utils


initCorpus :: Tokenizer (Token p PlainToken) -> DocumentReader b () -> Corpus b p
initCorpus = Corpus M.empty

makeCorpus :: Foldable t
           => Tokenizer (Token p PlainToken)
           -> DocumentReader b () -> t (Document b ()) -> Corpus b p
makeCorpus tokenizer reader docs =
    Corpus (foldl' insert M.empty docs) tokenizer reader
    where
        insert m d = M.insert (documentKey d) d m

addDocument :: Corpus b p -> Document b () -> Corpus b p
addDocument c d =
    c & corpusDocuments .~ M.insert (documentKey d) d (_corpusDocuments c)

updateDocumentFilters :: Corpus (Token p PlainToken) p -> IO (Corpus (Token p PlainToken) p)
updateDocumentFilters c = fmap (flip (set corpusDocuments) c . M.fromList)
                        . mapM (sequenceA . (documentKey &&& readDocumentTypes c))
                        . M.elems
                        $ c ^. corpusDocuments

loadCorpusDirectory :: Tokenizer (Token p PlainToken)
                    -> DocumentReader b ()
                    -> DocumentTransformer b ()
                    -> FilePath
                    -> IO (Corpus b p)
loadCorpusDirectory tokenizer reader transform root =
        fmap (makeCorpus tokenizer reader)
    .   mapM (transform . initDocument)
    =<< walk
    =<< makeAbsolute root
    where
        walk filename = do
            isDir <- doesDirectoryExist filename
            if isDir then walkDirectory filename else return [filename]

indexCorpus :: Corpus b p -> IO (IxIndex PlainToken, [Document Int [Token p Int]])
indexCorpus c = fmap (mapAccumL indexDocumentTokens I.empty)
              . mapM (tokenizeDocument c)
              . M.elems
              $ _corpusDocuments c

indexCorpus' :: Corpus b p -> IO (IxIndex PlainToken)
indexCorpus' c = foldlM (fmap (fmap fst) . readIndexDocumentTokens c) I.empty
               . M.elems
               $ _corpusDocuments c

inverseIndexCorpus :: Corpus b p -> IO (InverseIndex PlainToken (DocumentPos p))
inverseIndexCorpus c = fmap mconcat
                     . mapM (readInverseIndexDocument c)
                     . M.elems
                     $ _corpusDocuments c

readCorpusVectors :: Maybe PlainToken -> Corpus LineToken LinePos
                  -> IO (M.HashMap DocumentId (VectorDoc LineToken))
readCorpusVectors mtarget corpus =
      fmap ( M.fromList
           . fmap (_documentId &&& fmap (V.fromList . posTokenIndex))
           )
    . mapM (tokenizeDocument corpus)
    . maybe id (filter . filterToken) mtarget
    . M.elems
    $ _corpusDocuments corpus

transformer :: StopWords -> DocumentTransformer LineToken ()
transformer stopwords =
    readDocumentTypes' (fmap (tokenizerStop stopwords) . reader)
