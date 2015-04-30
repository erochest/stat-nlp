

module StatNLP.Utils where


import           Control.Applicative
import           Data.Traversable
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import           Prelude                   hiding (FilePath)


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    result <- f x
    (trues, falses) <- partitionM f xs
    return $ if result
                 then (x:trues, falses)
                 else (trues, x:falses)

walkDirectory :: FilePath -> IO [FilePath]
walkDirectory root = do
    children <- fmap (root </>) <$> listDirectory root
    (dirs, files) <- partitionM isDirectory children
    (files ++) <$> fmap concat (mapM walkDirectory dirs)
