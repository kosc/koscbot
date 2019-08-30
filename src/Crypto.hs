{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto ( cryptoRates
              , formatRates
              ) where

import GHC.Generics (Generic)
import Database (getCurrencies, getCryptoCurriencies)
import Database.PostgreSQL.Simple
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Monoid
import Data.String
import Data.Text (Text)
import Language.Haskell.TH
import Network.HTTP.Conduit (simpleHttp)
import Templates.Rates (mkRates, Rate(..))

$(do 
  currencies <- runIO getCryptoCurriencies
  mkRates $ map (map toLower) currencies)

$(deriveJSON defaultOptions{fieldLabelModifier = map toUpper} ''Rate)
$(deriveJSON defaultOptions{fieldLabelModifier = map toUpper} ''Rates)

makeUrl :: IO String
makeUrl = do
  cryptoCurrencties <- getCryptoCurriencies
  currencies <- getCurrencies
  return $ "https://min-api.cryptocompare.com/data/pricemulti?fsyms="++(intercalate "," cryptoCurrencties)++"&tsyms="++(intercalate "," currencies)

cryptoRates :: IO (Maybe Rates)
cryptoRates = do
  url <- makeUrl
  res <- simpleHttp url
  return (decode res :: Maybe Rates)

formatRates :: Rates -> Text
formatRates rates = fromString $ "```\n" ++ rateToString "BTC" (btc rates) ++ rateToString "ETH" (eth rates) ++ rateToString "XMR" (xmr rates) ++ rateToString "MET" (met rates) ++ "```"

rateToString :: String -> Rate -> String
rateToString coin rate = coin ++ ":\n    " ++ show (usd rate) ++ " USD\n    " ++ show (rur rate) ++ " RUR\n"
