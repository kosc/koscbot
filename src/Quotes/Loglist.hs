{-# LANGUAGE DeriveGeneric #-}
module Quotes.Loglist ( loglistQuote
                      , content
                      ) where

import Network.HTTP.Conduit 
import Data.Aeson
import GHC.Generics

data LoglistQuote = LoglistQuote
  { id :: String
  , source :: String
  , sourceUrl :: Maybe String
  , time :: String
  , content :: String
  , rating :: Int
  } deriving (Generic, Show)

instance FromJSON LoglistQuote

loglistQuote :: IO (Maybe LoglistQuote)
loglistQuote = do
  res <- simpleHttp "https://loglist.net/api/quote/random"
  return (decode res :: Maybe LoglistQuote)
