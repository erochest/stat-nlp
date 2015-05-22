{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Arrow           ((&&&))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.BloomFilter.Easy
import qualified Data.BloomFilter.Easy   as BF
import qualified Data.ByteString         as B
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Data.Text.Encoding      (decodeLatin1)
import qualified Data.Text.Format        as F
import qualified Data.Text.IO            as TIO
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Lazy.Builder  (toLazyText)
import           Data.Time
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector             as V
import           GHC.Exts                hiding (toList)
import           Taygeta.Tokenizer       (regexTokenizer)

import           Debug.Trace

import           StatNLP.Corpus
import           StatNLP.Document
import           StatNLP.Output
import           StatNLP.Output.Kwic
import           StatNLP.Text.Collocates
import           StatNLP.Text.Index      hiding (toList)
import           StatNLP.Text.Tokens
import           StatNLP.Text.Utils
import           StatNLP.Types
import           StatNLP.Utils

import           Opts

{-
 - **TODO**: Research Wittgenstein's *use theory of meaning*.
 -
 - TODO: probability distributions
 -}



main :: IO ()
main = do
    (corpusPath, mtarget) <- parseArgs

    -- stopwords
    stopwords <-  S.fromList . map (_tokenNorm . normalize) . tokenize
              <$> TIO.readFile "corpora/stopwords/english"

    let reader    = fmap decodeLatin1 . B.readFile . _documentId
        tokenizer = filter (not . (`S.member` stopwords) . _tokenNorm)
                  . fmap normalize
                  . tokenize
        docToken  = readDocumentTypes' ( fmap (tokenizer . decodeLatin1)
                                       . B.readFile
                                       . _documentId
                                       )
        -- docToken  = return

    F.print "Reading corpus from {}\n" $ F.Only corpusPath
    corpus <- loadCorpusDirectory tokenizer reader docToken corpusPath
    let setTokenI i t = t & tokenPos .~ i
        filterToken target Document{_documentTypes} =
            maybe True (token `BF.elem`) _documentTypes
            where
                token = Token target Nothing . Line 0 0 $ T.length target
    docs <- fmap ( M.fromList
                 . fmap (   _documentId
                        &&& fmap (V.fromList . zipWith (set tokenPos) ([0..] :: [Int]))
                        ))
         .  mapM (tokenizeDocument corpus)
         .  maybe id (filter . filterToken) mtarget
         .  M.elems
         $  _corpusDocuments corpus
    F.print "Documents potentially containing '{}' : {} / {}\n"
            (fromMaybe "0" mtarget, L.length docs, M.size (_corpusDocuments corpus))

    let InverseIndex index  = foldMap inverseIndexDocument docs
        targets             = L.sort $ maybe (M.keys index) return mtarget
        flatten ((a, b), c) = (a, b, c)

    forM_ targets $ \target -> do
        mapM_ (F.print "{} {}\t{}\n")
            . fmap (flatten . fmap getSum)
            . L.sortBy (comparing (Down . snd))
            . M.toList
            . unHash
            . foldMap ( frequencies
                      . uncurry (collocatesAround 0 3)
                      . fmap (fmap _tokenNorm . _documentTokens)
                      )
            . mapMaybe (sequenceA . second (`M.lookup` docs) . swap)
            $ M.lookupDefault [] target index

        putStrLn ""


freqShowReport :: (Traversable t, Show a) => Int -> t a -> IO ()
freqShowReport n = freqReport n . frequencies . fmap show

time :: NFData a => IO a -> IO a
time m = do
    start <- getCurrentTime
    a     <- m
    end   <- a `deepseq` getCurrentTime
    F.print "Elapsed time: {}\n\n" . F.Only . F.Shown $ end `diffUTCTime` start
    return a
