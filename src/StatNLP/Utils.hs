{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Utils where


import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Par
import           Data.Bifunctor
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Format    as F
import           Data.Time
import           System.Directory
import           System.FilePath


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    result <- f x
    (trues, falses) <- partitionM f xs
    return $ if result
                 then (x:trues, falses)
                 else (trues, x:falses)

walkDirectory :: FilePath -> IO [FilePath]
walkDirectory = walk <=< makeAbsolute
    where
        walk :: FilePath -> IO [FilePath]
        walk root = do
            children      <-  fmap (root </>) . filter (not . isHidden)
                          <$> getDirectoryContents root
            (dirs, files) <-  partitionM doesDirectoryExist children
            (files ++) . concat <$> mapM walk dirs

        isHidden []      = True
        isHidden ('.':_) = True
        isHidden _       = False

time :: NFData a => IO a -> IO a
time m = do
    start <- getCurrentTime
    a     <- m
    end   <- a `deepseq` getCurrentTime
    F.print "Elapsed time: {}\n\n" . F.Only . F.Shown $ end `diffUTCTime` start
    return a

foldParMap :: (Traversable t, Monoid b, NFData b) => (a -> b) -> t a -> b
foldParMap f = fold . runPar . parMap f

foldParMapChunk :: (Monoid b, NFData b)
                => Int -> (a -> b) -> [a] -> b
foldParMapChunk chunkSize f xs =
    runPar $   mapM (spawnP . foldMap f) (chunksOf chunkSize xs)
           >>= mapM get
           >>= spawnP . mconcat
           >>= get

third :: (a, b, c) -> c
third (_, _, c) = c

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

lookup' :: (Num n, Eq a, Hashable a) => M.HashMap a (Sum n) -> a -> n
lookup' m k = getSum $ M.lookupDefault mempty k m

readHashMap :: (Eq k, Hashable k, Read k, Read v) => FilePath -> IO (M.HashMap k v)
readHashMap filename =   M.fromList
                     .   mapMaybe ( fmap (read `bimap` read)
                                  . pair
                                  . splitWhen (== '\t')
                                  )
                     .   lines
                     <$> readFile filename

pair :: [a] -> Maybe (a, a)
pair [a, b] = Just (a, b)
pair _      = Nothing
