module Main where

import Data.String (fromString)
import Lib (run)
import System.Environment (lookupEnv)
import Telegram.Bot.API

main :: IO ()
main = do
  botToken <- lookupEnv "BOT_TOKEN"
  let token = case botToken of
              Just a -> Token $ fromString a
              Nothing -> error "You must define BOT_TOKEN environment variable"
  run token
