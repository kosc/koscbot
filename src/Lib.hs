{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import System.Environment
import Data.Text 
import Data.Maybe
import Data.String hiding (unlines, words)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Quotes.Loglist
import Quotes.Ibash
import Crypto
import Prelude hiding (unlines, head, words)

someFunc :: IO ()
someFunc = do
  bot_token <- lookupEnv "BOT_TOKEN"
  let token = case bot_token of
              Just a -> Token $ fromString a
              Nothing -> error "Invalid token"
  run token

data Model = Model {}

data Action
  = NoOp
  | Start
  | Crypto
  | Loglist
  | Ibash
  deriving (Show, Read)

initialModel :: Model
initialModel = Model {}

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

blacklist = ["tav0x222"]

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update = 
  let croppedMessage = fromJust . updateMessageText $ update
      username = fromJust $ userUsername . fromJust . messageFrom . fromJust $ updateMessage update
      parser = if elem username blacklist then UpdateParser {runUpdateParser = \update -> Nothing} else
            Start   <$ command "start"
        <|> Crypto  <$ command "start@koscbot"
        <|> Crypto  <$ command "crypto"
        <|> Crypto  <$ command "crypto@koscbot"
        <|> Loglist <$ command "loglist"
        <|> Loglist <$ command "loglist@koscbot"
        <|> Ibash   <$ command "ibash"
        <|> Ibash   <$ command "ibash@koscbot"
  in parseUpdate parser update

replyMarkdown :: Text -> BotM ()
replyMarkdown message = reply $ ReplyMessage message (Just Markdown) Nothing Nothing Nothing Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  Start -> model <# do
    replyText helpMessage
    return NoOp
  Crypto -> model <# do
    res <- liftIO cryptoRates
    replyMarkdown $ case res of
      Just rates -> formatRates rates
      Nothing -> "Can't get crypto rates."
    return NoOp
  Loglist -> model <# do
    res <- liftIO loglistQuote
    replyMarkdown $ case res of
      Just quote -> fromString $ content quote
      Nothing -> "Can't get quote from loglist.net"
    return NoOp
  Ibash -> model <# do
    res <- liftIO ibashQuote
    replyMarkdown $ fromString res
    return NoOp
  where helpMessage = unlines ["Simple telegram bot. Writen in Haskell."
                              , "Source code can be found at https://github.com/kosc/koscbot"
                              , "Any contributions are welcome."
                              ]
