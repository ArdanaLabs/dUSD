module Main
  ( main
  ) where

import Contract.Prelude
import UnitTest (helloUnitTest)
import Contract.Monad
  ( launchAff_
  , runContract_
  , configWithLogLevel
  )
import Contract.Wallet.KeyFile(mkKeyWalletFromFile)
import Serialization.Address (NetworkId(TestnetId))
import Data.Log.Level (LogLevel(Trace))



--import Wallet(mkNamiWalletAff)
--import Wallet(mkKeyWalletFromFile)
--import Wallet.KeyFile (privateKeyFromFile)
--import Wallet(mkKeyWallet)

main :: Effect Unit
main = launchAff_ $ do
  wallet' <- mkKeyWalletFromFile "wallet.skey"
  --wallet <- Just <$> mkNamiWalletAff
  --wallet <- map mkKeyWallet <$> privateKeyFromFile "./key"
  case wallet' of
    Just wallet -> do
      cfg <- configWithLogLevel TestnetId wallet Trace
      --cfg <- over ContractConfig _ { wallet = wallet } <$> traceTestnetContractConfig
      runContract_ cfg helloUnitTest
    Nothing -> undefined
