module Main
  ( main
  ) where

import Contract.Prelude
import UnitTest (helloUnitTest)
import Contract.Wallet (mkNamiWalletAff)
import Contract.Monad
  ( ContractConfig(ContractConfig)
  , launchAff_
  , runContract_
  , traceTestnetContractConfig
  )

main :: Effect Unit
main = launchAff_ $ do
  wallet' <- mkKeyWalletFromFile "wallet.skey"
  case wallet' of
  Just wallet -> do
    cfg <- configWithLogLevel TestnetId wallet Trace
    runContract_ cfg helloUnitTest
  Nothing -> undefined
  runContract_ cfg helloUnitTest
