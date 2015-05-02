{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Output.Kwic where


import           Control.Monad
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as T
import qualified Data.Text.Format          as F
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder
import           Filesystem.Path.CurrentOS

import           StatNLP.Document
import           StatNLP.Types


formatKwic :: Corpus LinePos
           -> Index PlainToken (DocumentPos LinePos)
           -> PlainToken
           -> IO T.Text
formatKwic corpus index token = do
    kwic' <- kwic corpus hits
    return .  TL.toStrict
           .  toLazyText
           $  F.build "{}\n{}\n" (token, T.replicate (T.length token) "=")
           <> kwic'
           <> F.build "N = {}\n\n" (F.Only . F.Shown $ length hits)
    where
        hits = M.lookupDefault [] token $ unIndex index

kwic :: Corpus LinePos -> [DocumentPos LinePos] -> IO Builder
kwic c pos = fmap fold
           . mapM (kwicGroup c)
           . L.groupBy (\a b -> fst a == fst b)
           $ L.sortBy (comparing fst) pos

kwicGroup :: Corpus LinePos -> [DocumentPos LinePos] -> IO Builder
kwicGroup c xs@((docId, _):_) = do
    case M.lookup (pathText docId) (corpusDocuments c) of
        Nothing -> return mempty
        Just d  ->  foldMap (kwicLine (pathText (filename docId)))
                .   syncLines xs'
                .   zip [0..]
                .   T.lines
                <$> corpusReader c d
    where
        pathText = either id id . toText
        xs' = L.groupBy (\a b -> posLine a == posLine b)
            . L.sort
            $ map snd xs
kwicGroup _ _ = return mempty

-- TODO: Need to wrap around the previous lines while normalizing
-- whitespace.
kwicLine :: T.Text -> (Int, T.Text, [LinePos]) -> Builder
kwicLine doc (lineNo, line, ps) = foldMap (sliceLine doc lineNo line) ps

sliceLine :: T.Text -> Int -> T.Text -> LinePos -> Builder
sliceLine doc lineNo line lp@(Line _ start end) =
    F.build "{}:{}: {}\t{}\t{}\n"
            (doc, F.left 4 ' ' (F.Shown lineNo), prefix, target, suffix)
    where
        (prefix, rest)   = T.splitAt start line
        (target, suffix) = T.splitAt (end - start) rest

syncLines :: [[LinePos]] -> [(Int, T.Text)] -> [(Int, T.Text, [LinePos])]
syncLines [] _ = []
syncLines _ [] = []
syncLines ([]:ps) ls = syncLines ps ls
syncLines allpos@((lp@(Line l _ _:_)):ps) ((lno, line):ls)
    | l == lno  = (lno, line, lp) : syncLines ps ls
    | otherwise = syncLines allpos ls
