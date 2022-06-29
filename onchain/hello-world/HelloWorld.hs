module Main (main) where

import Hello

import Utils(CBOR(..),toPureScript)
import Control.Monad (unless)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die)

{- | Main takes a directory as a comand line argument
  and creates a file CBOR.purs in that directory
  which will provide variables as configured in
  the cbors constant
-}
main :: IO ()
main = do
  getArgs >>= \case
    [out] -> do
      exists <- doesDirectoryExist out
      unless exists $ die $ "directory: " <> out <> " does not exist"
      writeFile (out ++ "/CBOR.purs") $
        "--this file was automatically generated by the onchain code\n"
        <> toPureScript cbors
    _ -> die "usage: cabal run hello-world <file_path>"

cbors :: [CBOR]
cbors =
  [ CBOR "paramHello" paramHelloCBOR
  , CBOR "hello" helloWorldHexString
  ]
