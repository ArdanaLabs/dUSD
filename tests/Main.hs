module Main (main) where

import Apropos.Plutus.Address qualified as Address
import Apropos.Plutus.AssetClass qualified as AssetClass
import Apropos.Plutus.Auction qualified as Auction
import Apropos.Plutus.Credential qualified as Credential
import Apropos.Plutus.Integer qualified as Integer
import Apropos.Plutus.SingletonValue qualified as SingletonValue
import Apropos.Plutus.StakingCredential qualified as StakingCredential
import Apropos.Plutus.Value qualified as Value
import Apropos.Plutus.Vault qualified as Vault

import Test.Syd
import Apropos (LogicalModel(scenarios))

-- TODO use sydtest-discover once nix stabalizes a bit more
-- TODO figure out why sydtest breaks the histograms and fix it

main :: IO ()
main = do
  print $ length $ scenarios @Vault.VaultProp
  sydTest $ do
    xdescribe "working but postponed for speed" $ do
      Address.spec
      AssetClass.spec
      Auction.spec
      Credential.spec
      Integer.spec
      SingletonValue.spec
      StakingCredential.spec
      Value.spec
    xdescribe "probably works but still too slow" Vault.spec
    describe "plutus" $ do
      Auction.spec
