

module StatNLP.Utils where


import           Control.Applicative
import           Control.Monad
import           Data.Traversable
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
            putStrLn $ "Walking " ++ root
            children      <-  fmap (root </>) . filter (not . isHidden)
                          <$> getDirectoryContents root
            (dirs, files) <-  partitionM doesDirectoryExist children
            (files ++) . concat <$> mapM walk dirs

        isHidden []      = True
        isHidden ('.':_) = True
        isHidden _       = False
