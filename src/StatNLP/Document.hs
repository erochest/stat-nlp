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


loadDocument :: (FilePath -> IO T.Text) -> Tokenizer (Token p) -> FilePath
             -> IO (Document p)
loadDocument reader tokenizer input =
    Document dId S.empty . V.fromList . tokenizer <$> reader input
    where
        dId = case toText input of
                  Left a  -> a
                  Right a -> a
