module Main
  ( main
  , hello
  ) where

import Contract.Prelude
import UnitTest (helloUnitTest)
import Contract.Wallet (mkNamiWalletAff)
import Contract.Monad
  ( ContractConfig(ContractConfig)
  , launchAff_
  , runContract_
  , traceTestnetContractConfig
  , logInfo'
  )

hello :: String -> Effect Unit
hello config = do
  log config
  pure unit

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- over ContractConfig _ { wallet = wallet } <$> traceTestnetContractConfig
  runContract_ cfg helloUnitTest
