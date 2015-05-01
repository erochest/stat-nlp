module StatNLP.Document where


import           Data.Either
import qualified Data.HashSet              as S
import qualified Data.Text                 as T
import           Data.Text.ICU
import qualified Data.Vector               as V
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           StatNLP.Text.Tokens
import           StatNLP.Types


initDocument :: DocumentId -> Document
initDocument input = Document input S.empty

documentKey :: Document -> T.Text
documentKey = either id id . toText . documentId
