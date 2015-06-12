module Opts where


import qualified Data.Text           as T
import           Options.Applicative


type Args = (FilePath, T.Text)

parseArgs :: IO Args
parseArgs = execParser opts

opts' :: Parser Args
opts' =   (,)
      <$> argument fileOpt (  metavar "CORPUS_DIR"
                           <> help "The root directory for the corpus files.")
      <*> argument textOpt (  metavar "TARGET_WORD"
                           <> help "The term to find major collocates for.")

opts :: ParserInfo Args
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Program for working through Foundations of Statistical NLP."
            <> header "stat-nlp")

fileOpt :: ReadM FilePath
fileOpt = str

textOpt :: ReadM T.Text
textOpt = T.pack <$> str
