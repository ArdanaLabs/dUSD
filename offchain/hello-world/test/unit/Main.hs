module Main (main) where

import Test.Tasty

import HelloWorld.Contract.ContractModels qualified (testTree)
import HelloWorld.Contract.EmulatorTraces qualified (testTree)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ testGroup "Emulator Traces" [HelloWorld.Contract.EmulatorTraces.testTree]
      , testGroup "ContractModels" [HelloWorld.Contract.ContractModels.testTree 10]
      ]
