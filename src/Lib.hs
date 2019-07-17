{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc,
      cryptoRates,
      formatRates,
      Rate,
      Rates,
    ) where

import           System.Environment
import           Data.Text
import           Data.String
import           GHC.Generics
import           Data.Aeson
import           Data.ByteString.Lazy.Internal (ByteString)
import           Network.HTTP.Conduit     (simpleHttp)
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Web.Telegram.API.Bot.Data

data Rate = Rate {
     usd :: Float,
     rur :: Float
} deriving (Generic, Show)

instance FromJSON Rate where
     parseJSON = withObject "Rate" $ \v -> Rate
                 <$> v .: "USD"
                 <*> v .: "RUR"

data Rates = Rates {
     btc :: Rate,
     eth :: Rate,
     xmr :: Rate
} deriving (Generic, Show)

instance FromJSON Rates where
     parseJSON = withObject "Rates" $ \v -> Rates
                 <$> v .: "BTC"
                 <*> v .: "ETH"
                 <*> v .: "XMR"

someFunc :: IO ()
someFunc = do
  manager <- newManager tlsManagerSettings
  res <- cryptoRates
  let message = case res of
                Just rates -> formatRates rates
                Nothing -> "Что-то пошло по пизде"
  chat_id <- lookupEnv "CHAT_ID"
  let chatId = case chat_id of
               Just a -> ChatChannel $ fromString a
  let request = sendMessageRequest chatId message 
  let myrequest = request { message_parse_mode = Just Markdown }
  bot_token <- lookupEnv "BOT_TOKEN"
  let token = case bot_token of
              Just a -> Token $ fromString a
  res <- sendMessage token myrequest manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right Response { result = m } -> do
      putStrLn "Request succeded"
      print $ message_id m
      print $ text m

cryptoRates :: IO (Maybe Rates)
cryptoRates = do 
              res <- simpleHttp "https://min-api.cryptocompare.com/data/pricemulti?fsyms=BTC,ETH,XMR&tsyms=USD,RUR"
              return (decode res :: Maybe Rates)

formatRates :: Rates -> Text
formatRates rates = fromString $ "```\n" ++ rateToString "BTC" (btc rates) ++ rateToString "ETH" (eth rates) ++ rateToString "XMR" (xmr rates) ++ "```"

rateToString :: String -> Rate -> String
rateToString coin rate = coin ++ ":\n    " ++ show (usd rate) ++ " USD\n    " ++ show (rur rate) ++ " RUR\n"
