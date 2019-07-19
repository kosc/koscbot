{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Crypto (
         cryptoRates
       , formatRates
       ) where

import GHC.Generics
import Data.String
import Data.Aeson
import Data.Text
import Network.HTTP.Conduit     (simpleHttp)

data Rate = Rate 
     { usd :: Float
     , rur :: Float
     } deriving (Generic, Show)

data Rates = Rates 
     { btc :: Rate
     , eth :: Rate
     , xmr :: Rate
     } deriving (Generic, Show)

instance FromJSON Rate where
     parseJSON = withObject "Rate" $ \v -> Rate
                 <$> v .: "USD"
                 <*> v .: "RUR"

instance FromJSON Rates where
     parseJSON = withObject "Rates" $ \v -> Rates
                 <$> v .: "BTC"
                 <*> v .: "ETH"
                 <*> v .: "XMR"

cryptoRates :: IO (Maybe Rates)
cryptoRates = do 
              res <- simpleHttp "https://min-api.cryptocompare.com/data/pricemulti?fsyms=BTC,ETH,XMR&tsyms=USD,RUR"
              return (decode res :: Maybe Rates)

formatRates :: Rates -> Text
formatRates rates = fromString $ "```\n" ++ rateToString "BTC" (btc rates) ++ rateToString "ETH" (eth rates) ++ rateToString "XMR" (xmr rates) ++ "```"

rateToString :: String -> Rate -> String
rateToString coin rate = coin ++ ":\n    " ++ show (usd rate) ++ " USD\n    " ++ show (rur rate) ++ " RUR\n"
