module Ctl.Utils.Test.Types (Mode(..)) where

import Prelude

data Mode = Local | Testnet

derive instance Eq Mode

