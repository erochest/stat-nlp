module StatNLP.Types
    ( PlainToken
    , PlainTokenizer
    , Tokenizer
    , FreqMap
    ) where


import           Conduit
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import           Taygeta.Types       (PlainToken, PlainTokenizer, Tokenizer)


type FreqMap a = M.HashMap a Int
