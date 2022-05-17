module Main (main) where

import Test.Tasty

-- import HelloWorld.ContractSpec qualified (testTree)
import HelloWorld.Contract.ContractModels qualified (tests)

main :: IO ()
main = do
  defaultMain $
    testGroup "ContractModels" [HelloWorld.Contract.ContractModels.tests 10]

    -- testGroup
    --   "Emulator Traces"
    --   [ HelloWorld.ContractSpec.testTree
    --   ]
