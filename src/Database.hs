{-# LANGUAGE OverloadedStrings #-}

module Database (
    dbConnection
  , getCurrencies
  , getCryptoCurrencies
) where

import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)

dbConnection :: IO Connection
dbConnection = do
  db_username <- fromMaybe "koscbot" <$> lookupEnv "DATABASE_USERNAME"
  db_name <- fromMaybe "koscbot" <$> lookupEnv "DATABASE_NAME"
  db_password <- fromMaybe "" <$> lookupEnv "DATABASE_PASSWORD"
  connect defaultConnectInfo {
    connectDatabase = db_name,
    connectUser = db_username,
    connectPassword = db_password
  }

getCurrencies :: IO [String]
getCurrencies = do
  conn <- dbConnection
  currencies <- query_ conn "select currency from currencies" :: IO [Only String]
  return $ map fromOnly currencies

getCryptoCurrencies :: IO [String]
getCryptoCurrencies = do
  conn <- dbConnection
  crypto_currencies <- query_ conn "select crypto_currency from crypto_currencies" :: IO [Only String]
  return $ map fromOnly crypto_currencies
