module Gen (
  address,
  assetClass,
  currencySymbol,
  datum,
  datumHash,
  datumOf,
  genData,
  hexString,
  hexString,
  hexString,
  integer,
  maybeOf,
  pos,
  pubKeyHash,
  rational,
  rationalRange,
  tokenName,
  txId,
  txId,
  txOutRef,
  validatorHash,
  value,
) where

import Apropos (Gen, choice, element, int, linear, list)

import Plutus.V1.Ledger.Api (
  Address (Address),
  BuiltinData (..),
  Credential (..),
  CurrencySymbol,
  Data,
  Datum (Datum),
  DatumHash,
  PubKeyHash,
  StakingCredential (..),
  TokenName,
  TxOutRef (TxOutRef),
  TxId (TxId),
  TxId (TxId),
  ValidatorHash,
  Value,
  singleton,
  toData,
 )
import PlutusTx.IsData.Class (ToData)

import Control.Monad (replicateM)
import Data.ByteString qualified as BS
import Data.Ratio
import Data.String (IsString (..))
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Builtins.Class (toBuiltin)

-- TODO address should get it's own apropos model
address :: Gen Address
address = Address <$> credential <*> maybeOf stakingCredential
  where
    stakingCredential :: Gen StakingCredential
    stakingCredential =
      choice
        [ StakingHash <$> credential
        , StakingPtr <$> integer <*> integer <*> integer
        ]

    credential :: Gen Credential
    credential =
      choice
        [ PubKeyCredential <$> pubKeyHash
        , ScriptCredential <$> validatorHash
        ]

hexString :: IsString s => Gen s
hexString = do
  len <- (2 *) <$> int (linear 1 32)
  fromString <$> replicateM len hexit

-- specific to tokenName and currencySymbol which can both be only 0 or 64 chars
hexStringName :: IsString s => Gen s
hexStringName = fromString <$> choice [pure "", replicateM 64 hexit]

currencySymbol :: Gen CurrencySymbol
currencySymbol = hexStringName

tokenName :: Gen TokenName
tokenName = hexStringName

assetClass :: Gen AssetClass
assetClass = Value.assetClass <$> currencySymbol <*> tokenName

pubKeyHash :: Gen PubKeyHash
pubKeyHash = hexString

validatorHash :: Gen ValidatorHash
validatorHash = hexString

datumHash :: Gen DatumHash
datumHash = hexString

txId :: Gen TxId
txId = do
  bytes <- replicateM 32 (fromIntegral <$> int (linear 0 255))
  return $ TxId $ toBuiltin $ BS.pack bytes

hexit :: Gen Char
hexit = element $ ['0' .. '9'] ++ ['a' .. 'f']

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = choice [pure Nothing, Just <$> g]

integer :: Gen Integer
integer = fromIntegral <$> int (linear (-1_000_000) 1_000_000)

pos :: Gen Integer
pos = fromIntegral <$> int (linear 1 1_000_000)

rational :: Gen Rational
rational = (%) <$> integer <*> pos

-- | Generate a value with lo < x < hi.
rationalRange :: Integer -> Rational -> Rational -> Gen Rational
rationalRange prec lo hi = do
  let prec' = fromIntegral prec
      iLo = fromInteger $ ceiling $ lo * prec'
      iHi = fromInteger $ floor $ hi * prec'
  val <- fromIntegral <$> int (linear iLo iHi)
  return $ val / prec'

datum :: Gen Datum
datum = Datum . BuiltinData <$> genData

genData :: Gen Data
genData =
  choice
    [ asData integer
    , asData value
    ]

value :: Gen Value
value = mconcat <$> list (linear 0 8) singletonValue
  where
    singletonValue :: Gen Value
    singletonValue =
      singleton <$> currencySymbol <*> tokenName <*> pos

asData :: ToData a => Gen a -> Gen Data
asData g = toData <$> g

datumOf :: ToData a => Gen a -> Gen Datum
datumOf g = Datum . BuiltinData . toData <$> g

txOutRef :: Gen TxOutRef
txOutRef = TxOutRef <$> hexString @TxId <*> pos
