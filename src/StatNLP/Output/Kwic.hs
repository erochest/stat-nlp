{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module StatNLP.Output.Kwic
    ( kwic
    , buildKwic
    , syncLines
    ) where


import           Control.Arrow             ((&&&))
import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import qualified Data.FingerTree           as FT
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Sequence             as Seq
import qualified Data.Text                 as T
import qualified Data.Text.Format          as F
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder
import           Filesystem.Path.CurrentOS

import           StatNLP.Context
import           StatNLP.Document
import           StatNLP.Types


type KwicContext = MeasuredContext T.Text
type KwicPending = (LinePos, T.Text, T.Text, KwicContext)
type KwicState   = (KwicContext, [KwicPending])

kwic :: Int
     -> Corpus LinePos
     -> Index PlainToken DocumentLine
     -> PlainToken
     -> IO [Kwic DocumentLine]
kwic context corpus index token =
    fmap L.concat . mapM (kwicDoc context corpus) $ sortGroup fst hits
    where
        hits = M.lookupDefault [] token $ unIndex index

buildKwic :: Kwic DocumentLine -> Builder
buildKwic Kwic{..} =
    let (docId, Line{..}) = kwicPos
        basename          = encodeString $ filename docId
    in   F.build "{}:{}:{}\t{}\t{}\t{}\n"
                 ( basename, posLine, posStart
                 , kwicPrefix, kwicTarget, kwicSuffix
                 )

kwicDoc :: Int -> Corpus LinePos -> [DocumentLine] -> IO [Kwic DocumentLine]
kwicDoc _ _ [] = return []
kwicDoc context corpus docs@((docId, _):_) =
    case M.lookup (either id id $ toText docId) (corpusDocuments corpus) of
        Nothing  -> return []
        Just doc ->  uncurry (++)
                 .   first (fmap (pendingKwic' docId) . snd)
                 .   concatMapAccum (stepLine docId context empty) (empty, [])
                 .   syncLines (sortGroup posLine $ fmap snd docs)
                 .   fmap (\(n, lineTokens) -> (n, fmap (updateLine n) <$> lineTokens))
                 .   zip [0..]
                 .   fmap (id &&& corpusTokenizer corpus)
                 .   T.lines
                 <$> corpusReader corpus doc
    where
        empty = emptyContext context

stepLine :: DocumentId
         -> Int
         -> KwicContext
         -> KwicState
         -> (Int, (T.Text, [Token LinePos]), [LinePos])
         -> (KwicState, [Kwic DocumentLine])
stepLine docId size empty s (lineNo, (line, tokens), hits) =
    concatMapAccum step s . syncHits tokens $ L.sortBy (comparing posStart) hits
    where
        step :: KwicState -> (Token LinePos, Bool) -> (KwicState, [Kwic DocumentLine])
        step (context, pending) (Token{tokenPos}, isHit) =
            ((context', pending'), map (pendingKwic' docId) current)
            where
                text     = T.take (posEnd tokenPos - posStart tokenPos)
                         $ T.drop (posStart tokenPos) line
                context' = text `pushLeft` context
                p        = (tokenPos, contextText context, text, empty)
                pending' = if isHit
                               then p:remaining
                               else remaining
                (current, remaining) = fmap (fmap (overContext (pushLeft text)))
                                     $ L.partition ( (>size)
                                                   . getSum
                                                   . FT.measure
                                                   . mContextSeq
                                                   . frth
                                                   ) pending

pendingKwic :: KwicPending -> Kwic LinePos
pendingKwic (linePos, pref, hit, context) =
    Kwic linePos pref hit $ contextText context

pendingKwic' :: DocumentId -> KwicPending -> Kwic DocumentLine
pendingKwic' docId = fmap (docId,) . pendingKwic

contextText :: KwicContext -> T.Text
contextText = T.intercalate " " . getContext

syncLines :: [[LinePos]]           -- ^ A document's hits, grouped and sorted by lines.
          -> [(Int, l)]            -- ^ A document's numbered lines.
          -> [(Int, l, [LinePos])]
syncLines [] xs = [(n, l, []) | (n, l) <- xs]
syncLines _  [] = []
syncLines ([]:ps) ls = syncLines ps ls
syncLines allpos@((lp@(Line l _ _:_)):ps) ((lno, line):ls)
    | l == lno  = (lno, line, lp) : syncLines ps ls
    | otherwise = (lno, line, []) : syncLines allpos ls

syncHits :: [Token LinePos] -> [LinePos] -> [(Token LinePos, Bool)]
syncHits [] _  = []
syncHits ts [] = map (,False) ts
syncHits (t@Token{tokenPos}:ts) hits =
    uncurry (:) . (((t,) . not . L.null) `bimap` syncHits ts) $ L.span (== tokenPos) hits

concatMapAccum :: (s -> a -> (s, [b])) -> s -> [a] -> (s, [b])
concatMapAccum _ s []     = (s, [])
concatMapAccum f s (a:as) = let (s', bs)   = f s a
                                (s'', bs') = concatMapAccum f s' as
                            in  (s'', bs ++ bs')

updateLine :: Int -> Token LinePos -> Token LinePos
updateLine n t = t { tokenPos = (tokenPos t) { posLine = n } }

sortGroup :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
sortGroup on = L.groupBy (\a b -> on a == on b) . L.sortBy (comparing on)

overContext :: (KwicContext -> KwicContext) -> KwicPending -> KwicPending
overContext f (a, b, c, d) = (a, b, c, f d)

frth :: (a, b, c, d) -> d
frth (_, _, _, a) = a
