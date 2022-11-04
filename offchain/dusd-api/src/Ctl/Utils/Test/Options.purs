module Ctl.Utils.Test.Options
  ( parser
  , Options(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Node.Path (FilePath)
import Options.Applicative (Parser, ParserInfo, fullDesc, header, help, helper, info, long, metavar, progDesc, short, strOption, flag', switch)
import Ctl.Utils.Test.Types (Mode(..))

data Options = Options
  { mode :: Mode
  , testResources :: FilePath
  , runVolumeTests :: Boolean
  }

parser :: ParserInfo Options
parser = info rawParser
  ( fullDesc
      <> progDesc "Execute CTL related tests."
      <> header "Test CLI"
  )

rawParser :: Parser Options
rawParser = (helper <*> _)
  $ map Options
  $ { mode: _, testResources: _, runVolumeTests: _ }
      <$> mode
      <*> testResources
      <*> runVolumeTests

testResources :: Parser FilePath
testResources = strOption $
  long "test-resources"
    <> short 'w'
    <> metavar "FILE_PATH"
    <> help "The path to the directory containting test-resources"

mode :: Parser Mode
mode = local <|> testnet

local :: Parser Mode
local = flag' Local $
  long "local"
    <> help "Run on local testnet."

testnet :: Parser Mode
testnet = flag' Testnet $
  long "testnet"
    <> help "Run on online tesnet."

runVolumeTests :: Parser Boolean
runVolumeTests = switch $
  long "run-volume-test"
    <> help "Whether to run volume-tests if there are some."
