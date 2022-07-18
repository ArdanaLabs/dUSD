module Main
  ( main
  ) where

import Contract.Prelude
import Effect.Aff(launchAff_)
import Options.Applicative(execParser)
import HelloWorld.CLI.Parser(parser)
import HelloWorld.CLI.Runners(runCLI)

main :: Effect Unit
main = do
  parsedCmd <- execParser parser
  launchAff_ $ runCLI parsedCmd
