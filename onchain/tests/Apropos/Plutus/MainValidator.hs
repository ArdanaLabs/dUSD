module Apropos.Plutus.MainValidator (
  ValidatorProp (..),
  ValidatorModel (..),
  spec,
) where

import Apropos
import Apropos.Script
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value qualified as V
import Data.Either (isLeft)
import Data.Maybe
import Control.Monad
import Data.String (IsString(fromString))
import Test.Syd (Spec)
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Plutarch
import Plutarch.Prelude
import Plutarch.Builtin (pforgetData)
import Validator (mainValidator,mainValidatorHash)

data ValidatorProp
  = HasConfig
  | ConfigIsValid
  | RedemerIsValid
  | MintsReferencedToken
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

data ValidatorModel = ValidatorModel
  { minting :: Value
  , inputs :: [TxInInfo]
  , redemer :: Either Int Data
  , configDatum :: Either [CurrencySymbol] (Maybe Data)
  , spending :: TxOutRef
  } deriving stock (Eq,Show)

nftcs :: CurrencySymbol
nftcs = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

instance LogicalModel ValidatorProp where
  logic =
    (Var MintsReferencedToken :->: (Var RedemerIsValid  :&&: Var ConfigIsValid))
    :&&:
    (Var ConfigIsValid :->: Var HasConfig)


instance HasLogicalModel ValidatorProp ValidatorModel where
  satisfiesProperty HasConfig m = any inputIsConfig . inputs $ m
  satisfiesProperty RedemerIsValid m = isLeft . redemer $ m
  satisfiesProperty ConfigIsValid m = isLeft . configDatum $ m
  satisfiesProperty MintsReferencedToken m = isJust $ do
    Left ind <- pure $ redemer m
    Left conf <- pure $ configDatum m
    guard $ length conf > ind
    guard $ V.assetClassValueOf (minting m) (V.AssetClass (conf !! ind,"")) > 0

inputIsConfig :: TxInInfo -> Bool
inputIsConfig (TxInInfo _ (TxOut _ v _)) = V.singleton nftcs "" 1 == v

instance HasPermutationGenerator ValidatorProp ValidatorModel where
  sources =
    [ Source
      { sourceName = "Valid"
      , covers = All $ Var <$> [HasConfig,ConfigIsValid,RedemerIsValid,MintsReferencedToken]
      , gen = do
          mintedCS <- genHexString @CurrencySymbol
          extra <- genVal :: Gen Value
          redemerInd <- int (linear 0 10) :: Gen Int
          extraConfLen <- int (linear 0 10) :: Gen Int
          -- TODO random input for this
          let spendingInput = TxOutRef (TxId "aa") 0
          config <- do
              pre <- replicateM redemerInd $ genHexString @CurrencySymbol
              post <- replicateM extraConfLen $ genHexString @CurrencySymbol
              pure $ pre ++ [mintedCS] ++ post
          pure $
            ValidatorModel
              (V.singleton mintedCS "" 1 <> extra)
              -- TODO generate random outref and more inputs
              [TxInInfo
                (TxOutRef (TxId "a") 0)
                (TxOut
                  (Address (ScriptCredential "") Nothing)
                  (V.singleton nftcs "" 1)
                  -- TODO should be hash of config datum
                  Nothing
                )
              , TxInInfo
                spendingInput
                (TxOut
                  (Address (ScriptCredential mainValidatorHash) Nothing)
                  -- TODO this should be a random value
                  mempty
                  -- TODO maybe this should be a hash of a random datum
                  Nothing
                )
              ]
              (Left redemerInd)
              (Left config)
              spendingInput
      }
    ]

  generators =
    [ Morphism
      { name = "invalidate redemer"
      , match = Var RedemerIsValid
      , contract = remove RedemerIsValid
      , morphism = \m -> do
          randomData <- genData
          pure $ m{redemer = Right randomData}
      }
    , Morphism
      { name = "remove config"
      , match = Var HasConfig
      , contract = remove HasConfig
      , morphism = \m -> do
          let inputs' = filter (not . inputIsConfig) (inputs m)
          pure $ m{configDatum = Right Nothing,inputs=inputs'}

      }
    , Morphism
      { name = "invalidate config"
      , match = Var ConfigIsValid
      , contract = remove ConfigIsValid
      , morphism = \m -> do
        randomData <- genData
        pure $ m{configDatum = Right $ Just randomData}

      }
    , Morphism
      { name = "don't mint referenced token"
      , match = Var MintsReferencedToken
      , contract = remove MintsReferencedToken
      , morphism = \m -> do
        newMintVal <- genVal
        -- TODO it might be better to filter out the minted value
        pure $ m{minting = newMintVal}
      }

    ]

instance HasParameterisedGenerator ValidatorProp ValidatorModel where
  parameterisedGenerator = buildGen

instance ScriptModel ValidatorProp ValidatorModel where
  expect = All $ Var <$> [HasConfig,ConfigIsValid,RedemerIsValid,MintsReferencedToken]
  script m = compile $ mainValidator
    # pforgetData (pdata (pconstant ()))
    # (case redemer m of
        Left n -> pforgetData $ pdata $ fromIntegral @Int @(Term _ PInteger) n
        Right da -> pconstant da
       )
    # pconstant
    (
      -- TODO use fraser's thing for this and deal with the other fields
      ScriptContext
        (TxInfo
          -- TODO most of these memptys are wrong
          (inputs m)
          [] -- TODO should there be outputs?
          mempty
          (minting m)
          []
          []
          (Interval (LowerBound NegInf False) (UpperBound PosInf False))
          []
          [] -- TODO we need the datum table
          (TxId "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        )
        (Spending $ spending m)
     )

genData :: Gen Data
genData = choice
  -- TODO this could stand some more choices
  -- also these should be in a seperate module
  [ toData <$> list (linear 0 10) (fromIntegral @Int @Integer <$> int (linear 0 10))
  , toData <$> Just <$> fromIntegral @Int @Integer <$> int (linear 0 10)
  ]

genHexString :: IsString s => Gen s
genHexString = fromString <$> replicateM 64 (element "0123456789abcdef")

genVal :: Gen Value
genVal = mconcat <$> list (linear 0 20) genSingleton

genSingleton :: Gen Value
genSingleton = V.singleton <$> genHexString @CurrencySymbol <*> genHexString @TokenName <*> (fromIntegral <$> int (linear (-1000) 1000))

spec :: Spec
spec = do
    fromHedgehogGroup $
      runGeneratorTestsWhere @ValidatorProp "generator" Yes
    fromHedgehogGroup $
      permutationGeneratorSelfTest @ValidatorProp

