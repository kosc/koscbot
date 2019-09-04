{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( run
    ) where

import Layout
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Database (dbConnection)
import Data.Text hiding (concat, map)
import Data.Maybe
import Data.Tuple.Only
import Data.String hiding (unlines, words)
import Database.PostgreSQL.Simple
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Quotes.Loglist
import Quotes.Ibash
import Crypto
import Prelude hiding (unlines, head, words)

data Model = Model {
  blacklist :: [String]
}

data Action
  = NoOp
  | Start
  | Crypto
  | Switch (Maybe Message)
  | Loglist
  | Ibash
  deriving (Show)

initialModel :: Model
initialModel = Model { blacklist = [] }

echoBot :: [String] -> BotApp Model Action
echoBot blist = BotApp
  { botInitialModel = initialModel { blacklist = blist }
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  black_list <- getBlacklist
  startBot_ (conversationBot updateChatId (echoBot black_list)) env

getBlacklist :: IO [String]
getBlacklist = do
  conn <- dbConnection
  users <- query_ conn "select username from blacklist" :: IO [Only String]
  return $ map fromOnly users

updateToAction :: Model -> Update -> Maybe Action
updateToAction model update = 
  let username = fromJust $ userUsername . fromJust . messageFrom . fromJust $ updateMessage update
      message = messageReplyToMessage . fromJust $ updateMessage update
      parser = if elem username (map pack (blacklist model)) then UpdateParser {runUpdateParser = \_ -> Nothing} else
        Start              <$ command "start"
        <|> Start          <$ command "start@koscbot"
        <|> Crypto         <$ command "crypto"
        <|> Crypto         <$ command "crypto@koscbot"
        <|> Switch message <$ command "switch" 
        <|> Switch message <$ command "switch@koscbot" 
        <|> Loglist        <$ command "loglist"
        <|> Loglist        <$ command "loglist@koscbot"
        <|> Ibash          <$ command "ibash"
        <|> Ibash          <$ command "ibash@koscbot"
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
    message <- liftIO $ formatCryptos res
    replyMarkdown $ fromString message
    return NoOp
  Switch message -> model <# do
    case message of
      Just msg -> replyMarkdown $ fromString $ changeLayout $ unpack (fromJust $ messageText msg)
      Nothing -> replyMarkdown $ fromString "Error! You should reply with this command to message with wrong layout."
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
