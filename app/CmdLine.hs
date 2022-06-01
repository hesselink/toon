module CmdLine where

import Options.Applicative

data Opts = Opts
  { configFile :: FilePath
  } deriving (Show, Eq)

opts :: Parser Opts
opts = Opts
  <$> strOption
      (  short 'c'
      <> long "config"
      <> metavar "CONFIG_FILE"
      <> help "Location of the configuration file."
      <> showDefault
      <> value "toon.yaml"
      )

optsWithInfo :: ParserInfo Opts
optsWithInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "Gather Toon data"
  <> header "toon - gather additional historical data from Toon"
  )

getOpts :: IO Opts
getOpts = execParser optsWithInfo
