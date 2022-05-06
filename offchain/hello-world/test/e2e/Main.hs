module Main (main) where

import Cardano.Wallet.Primitive.Types qualified as CTypes
import Control.Concurrent qualified as CC
import Control.Concurrent.Async qualified as Async
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON, Value)
import Data.Aeson qualified as Asn
import Control.Monad (void)
import Data.Functor (($>))
import Data.Word (Word16)
import HelloWorld.LocalCluster qualified as LC
import HelloWorld.PAB (HelloWorldContracts (..))
import Ledger.Value (CurrencySymbol (..))
import Network.HTTP.Client qualified as HTTP
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (err))
import Plutus.PAB.Webserver.Client (PabClient)
import Plutus.PAB.Webserver.Client qualified as PABClient
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs), ContractInstanceClientState (cicCurrentState))
import Servant.Client (
  BaseUrl (..),
  ClientEnv (..),
  Scheme (..),
 )
import Servant.Client qualified as SC
import System.Exit qualified as SysEx
import Test.Syd (
  TestDefM,
  describe,
  itWithOuter,
  sydTest,
 )
import Test.Syd qualified as SydT
import Test.Syd.Def (SetupFunc (..))
import Test.Syd.Expectation qualified as SydEx
import Text.Read qualified as TR
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Emulator.Wallet qualified as WTypes
import Wallet.Types (ContractInstanceId)
import Plutus.PAB.Events.ContractInstanceState
    ( PartiallyDecodedResponse(observableState) )

type LocalClusterSpec = TestDefM '[TestArgs] () ()

data TestArgs = MkTestArgs
  { networkManager :: HTTP.Manager
  , wallet :: Wallet
  }

main :: IO ()
main =
  -- runCluster does not terminate, so we need to handle it with race_
  Async.race_ LC.runCluster $
    sydTest specWithArgs
  where
    specWithArgs = SydT.setupAroundAll setupTestArgs spec

spec :: LocalClusterSpec
spec = describe "End to End" $ do
  itWithOuter "Runs Contract" $
    \MkTestArgs {networkManager, wallet} -> do
      let env = SC.mkClientEnv networkManager pabBaseUrl
          pclientInitialize = PABClient.pabClient @HelloWorldContracts @()
      contractId <- activatesContract env pclientInitialize wallet
      st <- usesEndpoint "initialize" (1 :: Integer) env pclientInitialize contractId
      putStrLn $ "ST: " <> show st
      pure ()

pabBaseUrl :: BaseUrl
pabBaseUrl =
  BaseUrl
    { baseUrlScheme = Http
    , baseUrlHost = localhost
    , baseUrlPort = pabBackendPort
    , baseUrlPath = ""
    }

activatesContract :: ClientEnv -> PabClient HelloWorldContracts walletId -> Wallet -> IO ContractInstanceId
activatesContract env pclient wallet = do
  let activationArgs = ContractActivationArgs Initialize (Just wallet)
      activateContract = PABClient.activateContract pclient activationArgs
  res <- SC.runClientM activateContract env
  case res of
    Left ce -> SydEx.expectationFailure $ "*** Failed to activate contract: " <> show ce
    Right cid ->
      verifyStatus env pclient cid $> cid

usesEndpoint ::
  ToJSON params =>
  String ->
  params ->
  ClientEnv ->
  PabClient t walletId ->
  ContractInstanceId ->
  IO Value
usesEndpoint endpoint params env pclient contractId = do
  let instanceClient = PABClient.instanceClient pclient contractId
      -- TODO: this function needs to be refactored to additionally allow for passing the correct endpoint parameter
      -- preferably to enforce the same type that the schema defines and not just value
      --params = Asn.toJSON @Integer 1
      req = PABClient.callInstanceEndpoint instanceClient endpoint (Asn.toJSON params)
  res <- SC.runClientM req env
  case res of
    Left ce -> reqErr ce
    Right _ -> verifyStatus env pclient contractId
  where
    errStr = "*** Failed on endpoint <" <> endpoint <> ">: "
    reqErr = SydEx.expectationFailure . (<>) errStr . show

{- | Verifies the contract status. This is necessary because the obvious way
 to check for errors -- i.e. if 'SC.runClientM' returns 'Left' -- is
 insufficient. For example, the PAB backend will happily return Right ()
 when we supply the wrong aeson params (e.g. 'Bool' when it's expecting
 ()).

 Luckily, these errors appear to be available on the status endpoint,
 hence this check.

 If the verification succeeds, we return the observable state json. Consumers
 can further check this state if desired.
-}
verifyStatus ::
  ClientEnv ->
  PabClient t walletId ->
  ContractInstanceId ->
  IO Value
verifyStatus env pclient contractId = do
  -- Pretty hacky, but evidently we need a delay whenever one endpoint must
  -- complete before the next ones can be called. Thus we call one before
  -- this function, which should be checked after each contract step
  sleepSeconds 5
  let instanceClient = PABClient.instanceClient pclient contractId
      req = PABClient.getInstanceStatus instanceClient
  res <- SC.runClientM req env
  case res of
    Left ce -> reqErr ce
    Right st -> do
      let currState = cicCurrentState st
       in maybe (pure (observableState currState)) statusErr (err currState)
  where
    reqErr = SydEx.expectationFailure . (<>) "*** Failed to get status: " . show
    statusErr = SydEx.expectationFailure . (<>) "*** Status error: " . show

setupTestArgs :: SetupFunc TestArgs
setupTestArgs = do
  netManager <- setupManager
  wallet <- setupWallet
  setupCluster netManager wallet

setupWallet :: SetupFunc Wallet
setupWallet = liftIO $ do
  case mWallet of
    Nothing -> SysEx.die "*** Error creating wallet digest"
    Just w -> pure w

setupManager :: SetupFunc HTTP.Manager
setupManager = liftIO (HTTP.newManager HTTP.defaultManagerSettings)

setupCluster :: HTTP.Manager -> Wallet -> SetupFunc TestArgs
setupCluster manager wallet = liftIO $ do
  clusterUp <- LC.waitCluster manager clusterTimeout localhost walletPort pabBackendPort
  if clusterUp
    then pure $ MkTestArgs manager wallet
    else SysEx.die $ "*** Cluster failed to start after " <> show clusterTimeout <> " seconds"

-- | Test wallet with hardcoded digest.
mWallet :: Maybe Wallet
mWallet = WTypes.Wallet (Just "HelloWorld") . WTypes.WalletId . CTypes.WalletId <$> digest
  where
    digest = TR.readMaybe "2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"

clusterTimeout :: Word16
clusterTimeout = 180

localhost :: String
localhost = "127.0.0.1"

walletPort :: Int
walletPort = 46493

pabBackendPort :: Int
pabBackendPort = 9080

-- | Sleeps for the given number of seconds
sleepSeconds :: Int -> IO ()
sleepSeconds = CC.threadDelay . (* 1_000_000)
