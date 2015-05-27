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


type StopWords   = S.HashSet PlainToken
type VectorDoc b = Document b (V.Vector (Token Int PlainToken))
type LineToken   = Token LinePos PlainToken

tokenizer :: Tokenizer LineToken
tokenizer = fmap normalize . tokenize

tokenizerStop :: StopWords -> Tokenizer LineToken
tokenizerStop stopwords = filter (not . (`S.member` stopwords) . _tokenNorm)
                        . tokenizer

reader :: Document b ts -> IO T.Text
reader = fmap decodeLatin1 . B.readFile . _documentId

transformer :: StopWords -> DocumentTransformer LineToken ()
transformer stopwords =
    readDocumentTypes' (fmap (tokenizerStop stopwords) . reader)

filterToken :: PlainToken -> Document LineToken ts -> Bool
filterToken norm =
    documentContains (Token norm Nothing . Line 0 0 $ T.length norm)

posTokenIndex :: [Token p t] -> [Token Int t]
posTokenIndex = zipWith (set tokenPos) [0..]

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

flatten :: ((a, b), (c, d, e)) -> (a, b, c, d, e)
flatten ((a, b), (c, d, e)) = (a, b, c, d, e)

printColls :: M.HashMap PlainToken [(DocumentId, Int)]
           -> M.HashMap DocumentId (VectorDoc b)
           -> T.Text
           -> IO ()
printColls index docs target =
      mapM_ (F.print "{}\t{}\t{}\t{}\t{}\n")
    . fmap flatten
    . L.sortBy (comparing (^. _2 . _3))
    . M.toList
    . collocateStats
    . concatMap ( uncurry (collocatesAround 0 3)
                . fmap (fmap _tokenNorm . _documentTokens)
                )
    . mapMaybe (sequenceA . second (`M.lookup` docs) . swap)
    $ M.lookupDefault [] target index

main :: IO ()
main = do
    (corpusPath, mtarget) <- parseArgs

    -- stopwords
    stopwords <-  S.fromList . fmap _tokenNorm . tokenizer
              <$> TIO.readFile "corpora/stopwords/english"

    F.print "Reading corpus from {}\n" $ F.Only corpusPath
    corpus <- loadCorpusDirectory tokenizer reader (transformer stopwords) corpusPath
    docs   <- readCorpusVectors mtarget corpus
    F.print "Documents potentially containing '{}' : {} / {}\n"
            (fromMaybe "0" mtarget, L.length docs, M.size (_corpusDocuments corpus))

    let InverseIndex index  = foldMap inverseIndexDocument docs
        targets             = L.sort $ maybe (M.keys index) return mtarget

    mapM_ (const (putStrLn "") <=< printColls index docs) targets


freqShowReport :: (Traversable t, Show a) => Int -> t a -> IO ()
freqShowReport n = freqReport n . frequencies . fmap show

time :: NFData a => IO a -> IO a
time m = do
    start <- getCurrentTime
    a     <- m
    end   <- a `deepseq` getCurrentTime
    F.print "Elapsed time: {}\n\n" . F.Only . F.Shown $ end `diffUTCTime` start
    return a
