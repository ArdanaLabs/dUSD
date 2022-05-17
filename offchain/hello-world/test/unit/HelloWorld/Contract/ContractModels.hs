{-# LANGUAGE TemplateHaskell            #-}
module HelloWorld.Contract.ContractModels (tests) where

import Control.Monad (void)
import Control.Lens hiding (elements)
import Data.Data 
import Data.Map qualified as M
import Data.Monoid (Last(..))
import Data.Maybe (isNothing, isJust, fromJust)
import Data.String (fromString)
import Data.Text qualified as T
import Plutus.V1.Ledger.Ada (adaValueOf)
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.Contract hiding (currentSlot, throwError, logInfo)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Plutus.Trace (callEndpoint, throwError)
import Plutus.Trace.Emulator (EmulatorRuntimeError(..), observableState, waitUntilSlot)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import HelloWorld.Contract (InitHelloWorldSchema, IncHelloWorldSchema, ReadHelloWorldSchema, initialize, increment', read'')

data HelloWorldModel = HelloWorldModel { 
  _helloWorldModel :: M.Map Wallet Integer
, _token :: Maybe SymToken 
} deriving (Eq, Data, Ord, Show)

makeLenses ''HelloWorldModel

instance ContractModel HelloWorldModel where
  -- Actions that will be generated by QuickCheck and can be performed on the hello world contract
  data Action HelloWorldModel =
      Initialize Wallet Integer
    | Increment Wallet
    | Read Wallet
    deriving (Data, Eq, Show)

  data ContractInstanceKey HelloWorldModel w s e p where
    InitializeKey :: Wallet -> Integer -> ContractInstanceKey HelloWorldModel (Last AssetClass ) InitHelloWorldSchema T.Text ()
    IncrementKey :: Wallet -> ContractInstanceKey HelloWorldModel () IncHelloWorldSchema T.Text ()
    ReadKey :: Wallet -> ContractInstanceKey HelloWorldModel (Last Integer) ReadHelloWorldSchema T.Text ()

  instanceContract :: (SymToken -> AssetClass) -> ContractInstanceKey HelloWorldModel w s e p -> p -> Contract w s e ()
  instanceContract _ (InitializeKey _ _) () = initialize
  instanceContract _ (IncrementKey _ ) () = increment'
  instanceContract _ (ReadKey _ ) () = read''

  instanceWallet :: ContractInstanceKey HelloWorldModel w s e p -> Wallet
  instanceWallet (InitializeKey w _) = w
  instanceWallet (IncrementKey w) = w
  instanceWallet (ReadKey w) = w

  instanceTag key = fromString $ "instance tag for: " <> show key

  arbitraryAction :: ModelState HelloWorldModel -> Gen (Action HelloWorldModel)
  arbitraryAction _ = oneof [
                        Initialize <$> genWallet <*> pure 0
                      , Increment <$> genWallet
                      , Read <$> genWallet
                      ]

  initialState :: HelloWorldModel
  initialState = HelloWorldModel M.empty Nothing

  initialInstances :: [StartContract HelloWorldModel]
  initialInstances = []

  precondition :: ModelState HelloWorldModel -> Action HelloWorldModel -> Bool
  precondition state (Initialize w _) = isNothing $ getHelloWorldModelState' state w
  precondition state (Increment w ) = isJust $ getHelloWorldModelState' state w
  precondition state (Read w ) = isJust $ getHelloWorldModelState' state w

  startInstances :: ModelState HelloWorldModel -> Action HelloWorldModel -> [StartContract HelloWorldModel]
  startInstances _ act  = case act of
    Initialize w _ -> [ StartContract (InitializeKey w 0) () ]
    Increment wi -> [ StartContract (IncrementKey wi ) () ]
    Read wr -> [ StartContract (ReadKey wr ) () ]

  -- Maps Actions to effects on the HelloWorldModel
  nextState :: Action HelloWorldModel -> Spec HelloWorldModel ()
  -- one slot will pass, the state will be initialized with i, minAdaValue will be spent
  nextState (Initialize wallet i) = do
    t <- createToken "foo"
    (helloWorldModel . at wallet) $= Just i
    token $= Just t
    withdraw wallet $ adaValueOf 2
    wait 4
  -- three slots will pass, the state will be incremented
  nextState (Increment wallet1 ) = do
    (helloWorldModel . ix wallet1) $~ (+ 1)
    wait 1
  -- three slots will pass, the state will be incremented, minAdaValue will be spent
  nextState (Read _) = do
    wait 1

  perform :: HandleFun HelloWorldModel -> (SymToken -> AssetClass) -> ModelState HelloWorldModel -> Action HelloWorldModel -> SpecificationEmulatorTrace ()
  perform h _ model (Initialize wallet i) = do
    let handle = h $ InitializeKey wallet i
    withWait 3 model $ callEndpoint @"initialize" handle i
    Last maybeAssetClass <- observableState handle
    maybe (throwError $ GenericError $ "starting token sale for wallet " <> show wallet <> " failed")
          (registerToken "foo")
          maybeAssetClass 
  perform h f model (Increment wallet1 ) = let ac = f $ fromJust $ model ^. contractState . token in withWait 1 model $ callEndpoint @"increment" (h $ IncrementKey wallet1 ) ac
  perform h f model (Read wallet1 ) = let ac = f $ fromJust $ model ^. contractState . token in withWait 1 model $ callEndpoint @"read" (h $ ReadKey wallet1 ) ac

deriving instance Eq (ContractInstanceKey HelloWorldModel w s e p)
deriving instance Show (ContractInstanceKey HelloWorldModel w s e p)

withWait :: Slot -> ModelState HelloWorldModel -> SpecificationEmulatorTrace () -> SpecificationEmulatorTrace ()
withWait n m c = void $ c >> waitUntilSlot ((m ^. currentSlot) + n)

genWallet :: Gen Wallet
genWallet = elements wallets

wallets :: [Wallet]
wallets = [w1, w2, w3]

getHelloWorldModelState' :: ModelState HelloWorldModel -> Wallet -> Maybe Integer
getHelloWorldModelState' state wallet = M.lookup wallet . _helloWorldModel $ state ^. contractState

-- getHelloWorldModelState :: Wallet -> Spec HelloWorldModel (Maybe Integer)
-- getHelloWorldModelState wallet = do
--   state <- getModelState
--   return $ getHelloWorldModelState' state wallet

-- propHelloWorldInitializesCorrect :: Actions HelloWorldModel -> Property
-- propHelloWorldInitializesCorrect = withMaxSuccess . propRunActions
--   (\state -> )

propHelloWorldIncremetsOne :: Actions HelloWorldModel -> Property
propHelloWorldIncremetsOne = propRunActions (const $ pure True)

propNoFundsLocked :: Property
propNoFundsLocked = checkNoLockedFundsProof defaultCheckOptions $ defaultNLFP @HelloWorldModel

tests :: Int -> TestTree
tests n = testGroup
   "HelloWorld Model"
     [ testProperty "No Funds Locked Check" $ withMaxSuccess n propNoFundsLocked
     , testProperty "No Errors Check" $ withMaxSuccess n propHelloWorldIncremetsOne]