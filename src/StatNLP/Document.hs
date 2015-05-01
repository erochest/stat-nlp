module StatNLP.Document where


import qualified Data.HashSet              as S
import qualified Data.Text                 as T
import           Data.Text.ICU
import qualified Data.Vector               as V
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           StatNLP.Text.Tokens
import           StatNLP.Types


loadDocument :: (FilePath -> IO T.Text) -> Tokenizer (Token SpanPos) -> FilePath
             -> IO Document
loadDocument reader tokenizer input = do
    text <- reader input
    return . Document dId S.empty text . V.fromList $! tokenizer text
    where
        dId = case toText input of
                  Left a  -> a
                  Right a -> a
