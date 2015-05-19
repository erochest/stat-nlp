{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module StatNLP.Output.Kwic
    ( kwic
    , buildKwic
    , syncLines
    , sliceHits
    , normWS
    ) where


import           Control.Arrow             ((&&&))
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
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


type KwicContext = MeasuredContext (T.Text, LinePos)
type KwicPending = (LinePos, T.Text, T.Text, KwicContext)
type KwicState   = (KwicContext, [KwicPending])

kwic :: Int
     -> Corpus LinePos
     -> InverseIndex PlainToken DocumentLine
     -> PlainToken
     -> IO [Kwic DocumentLine]
kwic context corpus index token =
    fmap L.concat . mapM (kwicDoc context corpus) $ sortGroup fst hits
    where
        hits = M.lookupDefault [] token $ unIndex index

buildKwic :: Int -> Kwic DocumentLine -> Builder
buildKwic context Kwic{..} =
    let (docId, Line{..}) = kwicPos
        basename          = encodeString $ filename docId
    in   F.build "{}:{}:{}  {}  **{}**  {}\n"
                 ( F.left 25 ' ' basename, F.left 7 ' ' posLine, F.left 3 ' ' posStart
                 , F.left context ' ' $ T.takeEnd context kwicPrefix
                 , kwicTarget, T.take context kwicSuffix
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
         -> (Int, (T.Text, [Token LinePos PlainToken]), [LinePos])
         -> (KwicState, [Kwic DocumentLine])
stepLine docId size empty s (lineNo, (line, tokens), hits) =
    concatMapAccum step s . syncHits tokens $ L.sortBy (comparing posStart) hits
    where
        step :: KwicState -> (Token LinePos PlainToken, Bool) -> (KwicState, [Kwic DocumentLine])
        step (context, pending) (Token{tokenPos}, isHit) =
            ((context', pending'), map (pendingKwic' docId) current)
            where
                text     = T.take (posEnd tokenPos - posStart tokenPos)
                         $ T.drop (posStart tokenPos) line
                citem    = (line, tokenPos)
                context' = citem `pushLeft` context
                start    = tokenPos { posEnd = posStart tokenPos }
                end      = tokenPos { posStart = posEnd tokenPos
                                    -- , posEnd   = posEnd tokenPos
                                    }
                p        = ( tokenPos
                           , contextText ((line, start) `pushLeft` context)
                           , text
                           , (line, end) `pushLeft` empty
                           )
                pending' = if isHit
                               then p:remaining
                               else remaining
                (current, remaining) =   fmap (overContext (pushLeft citem))
                                     <$> L.partition ((>size) . contextSize . frth) pending

pendingKwic :: KwicPending -> Kwic LinePos
pendingKwic (linePos, pref, hit, context) =
    Kwic linePos pref hit $ contextText context

pendingKwic' :: DocumentId -> KwicPending -> Kwic DocumentLine
pendingKwic' docId = fmap (docId,) . pendingKwic

contextText :: KwicContext -> T.Text
contextText = normWS . T.intercalate " " . sliceHits . getContext

sliceHits :: [(T.Text, LinePos)] -> [T.Text]
sliceHits [] = []
sliceHits ((l, Line n start _):rest) =
    reverse . map snd $ go n [(start, T.drop start l)] rest
    where
        go :: Int -> [(Int, T.Text)] -> [(T.Text, LinePos)] -> [(Int, T.Text)]
        go lineNo parts [] = parts
        go lineNo parts@((s, l'):parts') [(l, Line n _ end)]
            | lineNo == n = (s, T.take (end - s) l'):parts'
            | otherwise   = (0, T.take end l):parts
        go lineNo parts ((l, Line n _ _):hits)
            | lineNo == n = go lineNo parts hits
            | otherwise   = go n ((0, l):parts) hits

syncLines :: [[LinePos]]           -- ^ A document's hits, grouped and sorted by lines.
          -> [(Int, l)]            -- ^ A document's numbered lines.
          -> [(Int, l, [LinePos])]
syncLines [] xs = [(n, l, []) | (n, l) <- xs]
syncLines _  [] = []
syncLines ([]:ps) ls = syncLines ps ls
syncLines allpos@((lp@(Line l _ _:_)):ps) ((lno, line):ls)
    | l == lno  = (lno, line, lp) : syncLines ps ls
    | otherwise = (lno, line, []) : syncLines allpos ls

syncHits :: [Token LinePos PlainToken] -> [LinePos] -> [(Token LinePos PlainToken, Bool)]
syncHits [] _  = []
syncHits ts [] = map (,False) ts
syncHits (t@Token{tokenPos}:ts) hits =
    uncurry (:) . (((t,) . not . L.null) `bimap` syncHits ts) $ L.span (== tokenPos) hits

concatMapAccum :: (s -> a -> (s, [b])) -> s -> [a] -> (s, [b])
concatMapAccum _ s []     = (s, [])
concatMapAccum f s (a:as) = let (s', bs)   = f s a
                                (s'', bs') = concatMapAccum f s' as
                            in  (s'', bs ++ bs')

updateLine :: Int -> Token LinePos PlainToken -> Token LinePos PlainToken
updateLine n t = t { tokenPos = (tokenPos t) { posLine = n } }

sortGroup :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
sortGroup on = L.groupBy (\a b -> on a == on b) . L.sortBy (comparing on)

overContext :: (KwicContext -> KwicContext) -> KwicPending -> KwicPending
overContext f (a, b, c, d) = (a, b, c, f d)

frth :: (a, b, c, d) -> d
frth (_, _, _, a) = a

normWS :: T.Text -> T.Text
normWS = T.intercalate " " . filter (not . T.null) . T.split isSpace
