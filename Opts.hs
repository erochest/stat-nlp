module Opts where


import           Data.Text           (Text, pack)
import           Options.Applicative


type Args = FilePath

parseArgs :: IO Args
parseArgs = execParser opts

opts' :: Parser Args
opts' = argument fileOpt (  metavar "CORPUS_DIR"
                         <> help "The root directory for the corpus files.")

opts :: ParserInfo Args
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Program for working through Foundations of Statistical NLP."
            <> header "stat-nlp")

fileOpt :: ReadM FilePath
fileOpt = str

textOpt :: ReadM Text
textOpt = pack <$> str
