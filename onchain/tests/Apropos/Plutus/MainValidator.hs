module Apropos.Plutus.MainValidator (
  ValidatorProp (..),
  ValidatorModel (..),
  spec,
) where

import Apropos
import Apropos.ContextBuilder
import Apropos.Script
import Control.Monad
import Data.Either (isLeft)
import Data.Maybe
import Gen
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value qualified as V
import Test.Syd (Spec)
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Plutarch.Api.V1 (mkValidator)
import Plutus.V1.Ledger.Scripts (applyValidator)
import Validator (mainValidator, mainValidatorHash)

import Control.Monad.Identity (Identity)
import Control.Monad.State

data ValidatorProp
  = HasConfig
  | ConfigIsValid
  | RedemerIsValid
  | MintsReferencedToken
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

data ValidatorModel = ValidatorModel
  { minting :: Value
  , configInput :: Maybe (Address, Value, Maybe Datum)
  , extraInputs :: [(Address, Value, Maybe Datum)]
  , redemer :: Either Int Data
  , spending :: TxOutRef
  }
  deriving stock (Eq, Show)

nftcs :: CurrencySymbol
nftcs = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

instance LogicalModel ValidatorProp where
  logic =
    (Var MintsReferencedToken :->: (Var RedemerIsValid :&&: Var ConfigIsValid))
      :&&: (Var ConfigIsValid :->: Var HasConfig)

instance HasLogicalModel ValidatorProp ValidatorModel where
  satisfiesProperty HasConfig m = isJust . configInput $ m
  satisfiesProperty RedemerIsValid m = isLeft . redemer $ m
  satisfiesProperty ConfigIsValid m =
    case m of
      ValidatorModel {configInput = Just (_, _, Just _)} -> True
      _ -> False
  satisfiesProperty MintsReferencedToken m = isJust $ do
    Left ind <- pure $ redemer m
    (_, _, maybeDatum) <- configInput m
    Datum (BuiltinData conf) <- maybeDatum
    cs <- fromData @[CurrencySymbol] conf
    guard $ length cs > ind
    guard $ V.assetClassValueOf (minting m) (V.AssetClass (cs !! ind, "")) > 0

instance HasPermutationGenerator ValidatorProp ValidatorModel where
  sources =
    [ Source
        { sourceName = "Valid"
        , covers = All $ Var <$> [HasConfig, ConfigIsValid, RedemerIsValid, MintsReferencedToken]
        , gen = do
            mintedCS <- hexString @CurrencySymbol
            extra <- value
            redemerInd <- int (linear 0 10) :: Gen Int
            extraConfLen <- int (linear 0 10) :: Gen Int
            -- TODO random input for this
            let spendingInput = TxOutRef (TxId "aa") 0
            config <- do
              pre <- replicateM redemerInd $ hexString @CurrencySymbol
              post <- replicateM extraConfLen $ hexString @CurrencySymbol
              pure $ pre ++ [mintedCS] ++ post
            pure $
              ValidatorModel
                (V.singleton mintedCS "" 1 <> extra)
                -- TODO generate random outref and more inputs
                ( Just
                    ( Address (ScriptCredential "") Nothing
                    , V.singleton nftcs "" 1
                    , Just $ Datum $ BuiltinData $ toData config
                    )
                )
                [
                  ( Address (ScriptCredential mainValidatorHash) Nothing
                  , -- TODO this should be a random value
                    mempty
                  , -- TODO maybe this should be random data?
                    Nothing
                  )
                ]
                (Left redemerInd)
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
            pure $ m {redemer = Right randomData}
        }
    , Morphism
        { name = "remove config"
        , match = Var HasConfig
        , contract = remove HasConfig
        , morphism = \m -> do
            pure $ m {configInput = Nothing}
        }
    , Morphism
        { name = "invalidate config"
        , match = Var ConfigIsValid
        , contract = remove ConfigIsValid
        , morphism = \m -> do
            randomData <- genData
            case configInput m of
              Nothing -> failWithFootnote "model error config was not valid"
              Just (adr, val, _) -> do
                pure $
                  m
                    { configInput =
                        Just (adr, val, Just $ Datum $ BuiltinData randomData)
                    }
        }
    , Morphism
        { name = "don't mint referenced token"
        , match = Var MintsReferencedToken
        , contract = remove MintsReferencedToken
        , morphism = \m -> do
            newMintVal <- value
            -- TODO it might be better to filter out the minted value
            pure $ m {minting = newMintVal}
        }
    ]

instance HasParameterisedGenerator ValidatorProp ValidatorModel where
  parameterisedGenerator = buildGen

instance ScriptModel ValidatorProp ValidatorModel where
  -- TODO for real validator the expect logic should be this:
  -- All $ Var <$> [HasConfig,ConfigIsValid,RedemerIsValid,MintsReferencedToken]
  expect = Yes

  script m =
    let ctx =
          buildContext $ do
            -- TODO this should be reworked in apropos-tx so the type hint is shorter
            withTxInfoBuilder @(StateT ScriptContext) @Identity @(StateT TxInfo) $ do
              addInput undefined undefined undefined undefined
            undefined m
     in applyValidator
          ctx
          (mkValidator mainValidator)
          ( Datum $
              BuiltinData $
                case redemer m of
                  Left n -> toData $ fromIntegral @Int @Integer n
                  Right da -> da
          )
          (Redeemer $ BuiltinData $ toData ())

{-
compile $
  mainValidator
    # pforgetData (pdata (pconstant ()))
    # ( case redemer m of
          Left n -> pforgetData $ pdata $ fromIntegral @Int @(Term _ PInteger) n
          Right da -> pconstant da
      )
    # pconstant
      ( -- TODO use fraser's thing for this and deal with the other fields
        ScriptContext
          ( TxInfo
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
      -}

spec :: Spec
spec = do
  fromHedgehogGroup $
    runGeneratorTestsWhere @ValidatorProp "generator" Yes
  fromHedgehogGroup $
    permutationGeneratorSelfTest @ValidatorProp
  fromHedgehogGroup $
    runScriptTestsWhere @ValidatorProp "nft script tests" Yes
