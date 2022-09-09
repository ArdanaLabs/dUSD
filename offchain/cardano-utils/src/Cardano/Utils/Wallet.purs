module Cardano.Utils.Wallet where

import Prelude

import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (singleton)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Path (FilePath, concat)

generateKeyPair :: FilePath -> FilePath -> String -> Effect (Tuple FilePath FilePath)
generateKeyPair nodeSocket outputDir keyName = do
  let sKeyPath = concat  [outputDir, keyName <> "skey"]
      vKeyPath = concat [outputDir, keyName <> "vkey"]
  void $ execSync ("cardano-cli address key-gen --verification-key-file " <> vKeyPath <> " --signing-key-file " <> sKeyPath)
                  defaultExecSyncOptions { env = Just (singleton "CARDANO_NODE_SOCKET_PATH" nodeSocket) }
  pure $ Tuple sKeyPath vKeyPath 
