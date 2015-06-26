module StatNLP.Input where


import qualified Data.ByteString    as B
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeLatin1)

import           StatNLP.Types


reader :: Document b ts -> IO Text
reader = fmap decodeLatin1 . B.readFile . _documentId
