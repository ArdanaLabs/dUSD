module Main (main) where

import qualified Apropos.Plutus.Address as Address
import qualified Apropos.Plutus.AssetClass as AssetClass
import qualified Apropos.Plutus.Auction as Auction
import qualified Apropos.Plutus.Credential as Credential
import qualified Apropos.Plutus.Integer as Integer
import qualified Apropos.Plutus.SingletonValue as SingletonValue
import qualified Apropos.Plutus.StakingCredential as StakingCredential
import qualified Apropos.Plutus.Value as Value
import qualified Apropos.Plutus.Vault as Vault

import Test.Syd
--import Apropos
--import Apropos.LogicalModel
--import Apropos.Plutus.Vault

-- TODO use sydtest-discover once nix stabalizes a bit more
-- TODO figure out why sydtest breaks the histograms and fix it

main :: IO ()
main =
  --print (length $ solveAll (logic :: Formula VaultProp))
  sydTest $
    describe "plutus" $ do
        Address.spec
        AssetClass.spec
        Auction.spec
        Credential.spec
        Integer.spec
        Integer.spec
        SingletonValue.spec
        StakingCredential.spec
        Value.spec
        Vault.spec
