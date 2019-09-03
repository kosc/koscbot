{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Crypto ( formatCryptos
              , cryptoRates
              ) where

import Control.Lens
import GHC.Generics (Generic)
import Database (getCurrencies, getCryptoCurriencies)
import Database.PostgreSQL.Simple
import Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Aeson
import Data.Aeson.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Text as T (pack, unpack)
import Network.HTTP.Conduit (simpleHttp)

makeUrl :: IO String
makeUrl = do
  cryptoCurrencties <- getCryptoCurriencies
  currencies <- getCurrencies
  return $ "https://min-api.cryptocompare.com/data/pricemulti?fsyms="++(intercalate "," cryptoCurrencties)++"&tsyms="++(intercalate "," currencies)

cryptoRates :: IO String
cryptoRates = do
  url <- makeUrl
  res <- simpleHttp url
  return $ Char8.unpack res

formatCryptos json = do
  cryptos <- getCryptoCurriencies
  currencies <- getCurrencies
  return $ "```\n" ++ (intercalate "" [formatCrypto (map T.pack currencies) crypto json | crypto <- cryptos]) ++ "```"

formatCurrency currency crypto json = "    " ++ (show $ fromJust $ json ^? key crypto . key currency . _Number) ++ " " ++ T.unpack currency

formatCrypto currencies crypto json = crypto ++ ":\n" ++ unlines [formatCurrency currency (T.pack crypto) json | currency <- currencies]
