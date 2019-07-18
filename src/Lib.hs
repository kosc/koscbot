{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc,
      cryptoRates,
      formatRates,
      Rate,
      Rates,
    ) where

import           System.IO.Unsafe
import           Control.Applicative
import           System.Environment
import           Data.Text
import           Data.Maybe
import           Data.String
import           GHC.Generics
import           Data.Aeson
import           Data.ByteString.Lazy.Internal (ByteString)
import           Network.HTTP.Conduit     (simpleHttp)
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser
import           Quotes

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
  chat_id <- lookupEnv "CHAT_ID"
  bot_token <- lookupEnv "BOT_TOKEN"
  let token = case bot_token of
              Just a -> Token $ fromString a
              Nothing -> error "Invalid token"
  run token

data Model = Model {
}

data Action
  = NoOp
  | Start
  | Crypto
  | Loglist
  deriving (Show, Read)

initialModel :: Model
initialModel = Model {
}

echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

run :: Token -> IO ()
run token = do
            env <- defaultTelegramClientEnv token
            startBot_ (conversationBot updateChatId echoBot) env

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ = parseUpdate $
               Start     <$ command "start"
           <|> Crypto    <$ command "crypto"
           <|> Crypto    <$ command "crypto@koscbot"
           <|> Loglist   <$ command "loglist"
           <|> Loglist   <$ command "loglist@koscbot"
           <|> callbackQueryDataRead

replyMarkdown text = reply $ ReplyMessage text (Just Markdown) Nothing Nothing Nothing Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  Start -> model <# do
        replyText "Help coming soon."
        return NoOp
  Crypto -> model <# do
        replyMarkdown message
        return NoOp
  Loglist -> model <# do
        replyMarkdown $ fromString loglistQuote
        return NoOp
  where
       message = formatRates $ (fromJust . unsafePerformIO) cryptoRates

cryptoRates :: IO (Maybe Rates)
cryptoRates = do 
              res <- simpleHttp "https://min-api.cryptocompare.com/data/pricemulti?fsyms=BTC,ETH,XMR&tsyms=USD,RUR"
              return (decode res :: Maybe Rates)

formatRates :: Rates -> Text
formatRates rates = fromString $ "```\n" ++ rateToString "BTC" (btc rates) ++ rateToString "ETH" (eth rates) ++ rateToString "XMR" (xmr rates) ++ "```"

rateToString :: String -> Rate -> String
rateToString coin rate = coin ++ ":\n    " ++ show (usd rate) ++ " USD\n    " ++ show (rur rate) ++ " RUR\n"
