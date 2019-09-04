{-# LANGUAGE OverloadedStrings #-}
module Crypto ( formatCryptos
              , cryptoRates
              , formatCurrency
              , formatCrypto
              ) where

import Control.Lens
import Database (getCurrencies, getCryptoCurrencies)
import Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Aeson.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Text as T (pack, unpack, Text)
import Network.HTTP.Conduit (simpleHttp)

makeUrl :: IO String
makeUrl = do
  cryptoCurrencties <- getCryptoCurrencies
  currencies <- getCurrencies
  return $ "https://min-api.cryptocompare.com/data/pricemulti?fsyms="++(intercalate "," cryptoCurrencties)++"&tsyms="++(intercalate "," currencies)

cryptoRates :: IO String
cryptoRates = do
  url <- makeUrl
  res <- simpleHttp url
  return $ Char8.unpack res

formatCryptos :: String -> IO String
formatCryptos json = do
  cryptos <- getCryptoCurrencies
  currencies <- getCurrencies
  return $ "```\n" ++ (intercalate "" [formatCrypto (map T.pack currencies) crypto json | crypto <- cryptos]) ++ "```"

formatCurrency :: AsValue s => T.Text -> T.Text -> s -> String
formatCurrency currency crypto json = "    " ++ (show $ fromJust $ json ^? key crypto . key currency . _Number) ++ " " ++ T.unpack currency

formatCrypto :: AsValue s => [T.Text] -> String -> s -> String
formatCrypto currencies crypto json = crypto ++ ":\n" ++ unlines [formatCurrency currency (T.pack crypto) json | currency <- currencies]
