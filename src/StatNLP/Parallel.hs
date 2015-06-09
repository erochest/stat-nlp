module StatNLP.Parallel
    ( parMapChunkM
    ) where


import           Control.Monad.IO.Class
import qualified Control.Monad.Par            as P hiding (runParIO)
import           Control.Monad.Par.Class
import           Control.Monad.Par.Combinator
import           Control.Monad.Par.IO
import           Control.Monad.Trans
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.List                    as L
import           Data.List.Split              (chunksOf)


parMapChunkM :: NFData b => Int -> (a -> IO b) -> [a] -> IO [b]
parMapChunkM chunk f = runParIO . fmap concat . parMapM (step f) . chunksOf chunk

step :: NFData b => (a -> IO b) -> [a] -> ParIO [b]
step f xs = do
    x <- new
    fork (liftIO (mapM f xs) >>= put x)
    get x
