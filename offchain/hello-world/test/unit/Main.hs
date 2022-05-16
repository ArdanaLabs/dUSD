module Main (main) where

import Test.Tasty

import HelloWorld.ContractSpec qualified (testTree)
import HelloWorld.Contract.ContractModels qualified (test)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Emulator Traces"
      [ HelloWorld.ContractSpec.testTree
      ]
  HelloWorld.Contract.ContractModels.test
