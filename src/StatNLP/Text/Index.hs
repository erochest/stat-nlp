module StatNLP.Text.Index where


import           Control.Arrow
import           Data.Hashable
import qualified Data.HashMap.Strict as M

import           StatNLP.Types


inverseIndex :: (Hashable k, Eq k) => (a -> k) -> (a -> p) -> [a] -> Index k p
inverseIndex key pos = Index . M.fromListWith (++) . map (key &&& (pure . pos))

indexTokens :: [Token LinePos] -> Index PlainToken LinePos
indexTokens = inverseIndex tokenNorm tokenPos
