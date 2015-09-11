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


import           Control.Arrow          ((&&&))
import           Control.Lens           hiding (parts)
import           Data.Bifunctor
import           Data.Char
import           Data.Function          (on)
import qualified Data.HashMap.Strict    as M
import qualified Data.List              as L
import           Data.Ord
import qualified Data.Text              as T
import qualified Data.Text.Format       as F
import           Data.Text.Lazy.Builder
import           System.FilePath

import           StatNLP.Context
import           StatNLP.Types


type KwicContext = MeasuredContext (T.Text, LinePos)
type KwicPending = (LinePos, T.Text, T.Text, KwicContext)
type KwicState   = (KwicContext, [KwicPending])

kwic :: Int
     -> Corpus b LinePos
     -> InverseIndex PlainToken DocumentLine
     -> PlainToken
     -> IO [Kwic DocumentLine]
kwic context corpus index token =
    fmap L.concat . mapM (kwicDoc context corpus) $ sortGroup fst hits
    where
        hits = M.lookupDefault [] token $ unIndex index

buildKwic :: Int -> Kwic DocumentLine -> Builder
buildKwic context Kwic{..} =
    let (docId, Line{..}) = _kwicPos
        basename          = takeFileName docId
    in  F.build "{}:{}:{}  {}  **{}**  {}\n"
                ( F.left 25 ' ' basename, F.left 7 ' ' _posLine, F.left 3 ' ' _posStart
                , F.left context ' ' $ T.takeEnd context _kwicPrefix
                , _kwicTarget, T.take context _kwicSuffix
                )

kwicDoc :: Int -> Corpus b LinePos -> [DocumentLine] -> IO [Kwic DocumentLine]
kwicDoc _ _ [] = return []
kwicDoc context corpus docs@((docId, _):_) =
    case M.lookup (T.pack docId) (_corpusDocuments corpus) of
        Nothing  -> return []
        Just doc ->  uncurry (++)
                 .   first (fmap (pendingKwic' docId) . snd)
                 .   concatMapAccum (stepLine docId context empty) (empty, [])
                 .   syncLines (sortGroup _posLine $ fmap snd docs)
                 .   fmap (\(n, lineTokens) -> (n, fmap (updateLine n) <$> lineTokens))
                 .   zip [0..]
                 .   fmap (id &&& _corpusTokenizer corpus)
                 .   T.lines
                 <$> _corpusReader corpus doc
    where
        empty = emptyContext context

stepLine :: DocumentId
         -> Int
         -> KwicContext
         -> KwicState
         -> (Int, (T.Text, [Token LinePos PlainToken]), [LinePos])
         -> (KwicState, [Kwic DocumentLine])
stepLine docId size empty s (lineNo, (line, tokens), hits) =
    concatMapAccum step s . syncHits tokens $ L.sortBy (comparing _posStart) hits
    where
        step :: KwicState -> (Token LinePos PlainToken, Bool) -> (KwicState, [Kwic DocumentLine])
        step (context, pending) (Token{_tokenPos}, isHit) =
            ((context', pending'), map (pendingKwic' docId) current)
            where
                text     = T.take (_posEnd _tokenPos - _posStart _tokenPos)
                         $ T.drop (_posStart _tokenPos) line
                citem    = (line, _tokenPos)
                context' = citem `pushLeft` context
                start    = _tokenPos & posEnd   .~ (_tokenPos ^. posStart)
                end      = _tokenPos & posStart .~ (_tokenPos ^. posEnd)
                p        = ( _tokenPos
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
        go lineNo parts@((s, l'):parts') [(l'', Line n' _ end)]
            | lineNo == n' = (s, T.take (end - s) l'):parts'
            | otherwise    = (0, T.take end l''):parts
        go lineNo parts ((l', Line n' _ _):hits)
            | lineNo == n' = go lineNo parts hits
            | otherwise    = go n' ((0, l'):parts) hits

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
syncHits (t@Token{_tokenPos}:ts) hits =
    uncurry (:) . (((t,) . not . L.null) `bimap` syncHits ts) $ L.span (== _tokenPos) hits

concatMapAccum :: (s -> a -> (s, [b])) -> s -> [a] -> (s, [b])
concatMapAccum _ s []     = (s, [])
concatMapAccum f s (a:as) = let (s', bs)   = f s a
                                (s'', bs') = concatMapAccum f s' as
                            in  (s'', bs ++ bs')

updateLine :: Int -> Token LinePos PlainToken -> Token LinePos PlainToken
updateLine n t = t & tokenPos . posLine .~ n

sortGroup :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
sortGroup f = L.groupBy ((==) `on` f) . L.sortBy (comparing f)

overContext :: (KwicContext -> KwicContext) -> KwicPending -> KwicPending
overContext f (a, b, c, d) = (a, b, c, f d)

frth :: (a, b, c, d) -> d
frth (_, _, _, a) = a

normWS :: T.Text -> T.Text
normWS = T.intercalate " " . filter (not . T.null) . T.split isSpace
