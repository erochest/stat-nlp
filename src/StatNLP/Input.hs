module StatNLP.Input where


import qualified Data.ByteString    as B
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeLatin1)

import           StatNLP.Types


reader :: Document b ts -> IO T.Text
reader = fmap decodeLatin1 . B.readFile . _documentId
