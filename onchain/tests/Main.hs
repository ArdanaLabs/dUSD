module Main (main) where

import Apropos.Plutus.AssetClass qualified as AssetClass
import Apropos.Plutus.Auction qualified as Auction
import Apropos.Plutus.HelloValidator qualified as HelloValidator
import Apropos.Plutus.Integer qualified as Integer
import Apropos.Plutus.NFT qualified as NFT
import Apropos.Plutus.SingletonValue qualified as SingletonValue
import Apropos.Plutus.Value qualified as Value
import Apropos.Plutus.Vault qualified as Vault
import Apropos.Plutus.VaultTransfer qualified as VaultTransfer

import Test.Syd

-- TODO use sydtest-discover once nix stabalizes a bit more
-- TODO figure out why sydtest breaks the histograms and fix it

main :: IO ()
main =
  sydTest $ do
    describe "plutus" $ do
      NFT.spec
      VaultTransfer.spec
      AssetClass.spec
      Integer.spec
      SingletonValue.spec
      Value.spec
      Vault.spec
      Auction.spec
      HelloValidator.spec
