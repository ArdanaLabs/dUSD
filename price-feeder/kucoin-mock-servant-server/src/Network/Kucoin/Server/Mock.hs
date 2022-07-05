{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Kucoin.Server.Mock (kucoinMockApp, mockFiatPriceHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, err400, err500, errBody, serve)

import Network.Kucoin.API (FiatPriceResponse (..), fiatPriceAPIProxy)
import PriceData (PriceData (..))

kucoinMockApp :: FilePath -> Application
kucoinMockApp priceDataPath = serve fiatPriceAPIProxy (mockFiatPriceHandler priceDataPath)

mockFiatPriceHandler :: FilePath -> Maybe T.Text -> [T.Text] -> Handler FiatPriceResponse
mockFiatPriceHandler priceDataPath (Just "USD") ["ADA"] = do
  eitherPriceData <- liftIO $ eitherDecodeFileStrict priceDataPath
  either
    (\msg -> throwError $ err500 {errBody = BSL.pack msg})
    (\price -> return $ FiatPriceResponse {_code = "200000", _data = M.singleton "ADA" $ T.pack $ show $ fromJust price})
    (_kucoinPrice <$> eitherPriceData)
mockFiatPriceHandler _ _ _ = throwError $ err400 {errBody = "Not Found."}