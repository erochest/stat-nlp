module Opts where


import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath)


parseArgs :: IO FilePath
parseArgs = execParser opts

opts' :: Parser FilePath
opts' = argument fileOpt (  metavar "CORPUS_DIR"
                         <> help "The root directory for the corpus files.")

opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Program for working through Foundations of Statistical NLP."
            <> header "stat-nlp")

fileOpt :: ReadM FilePath
fileOpt = decodeString <$> str
