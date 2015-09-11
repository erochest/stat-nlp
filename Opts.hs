module Opts where


import           Data.Text           (Text, pack)
import           Options.Applicative


data Args
        = Args
        { inputDir :: !FilePath
        , stopList :: !(Maybe FilePath)
        } deriving (Show, Eq)

parseArgs :: IO Args
parseArgs = execParser opts

opts' :: Parser Args
opts' =   Args
      <$> option fileOpt (  short 'c' <> long "corpus-dir" <> metavar "CORPUS_DIR"
                         <> help "The root directory for the corpus files.")
      <*> optional (option fileOpt (  short 's' <> long "stoplist" <> metavar "STOPLIST_FILE"
                                   <> help "A file to use as a stoplist."))

opts :: ParserInfo Args
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Program for working through Foundations of Statistical NLP."
            <> header "stat-nlp")

fileOpt :: ReadM FilePath
fileOpt = str

textOpt :: ReadM Text
textOpt = pack <$> str
