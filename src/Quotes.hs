{-# LANGUAGE DeriveGeneric #-}
module Quotes 
       ( loglistQuote
       ) where

import Network.HTTP.Conduit 
import Data.Aeson
import Data.Maybe
import GHC.Generics
import System.IO.Unsafe

data LoglistQuote = LoglistQuote {
     id :: String,
     source :: String,
     sourceUrl :: Maybe String,
     time :: String,
     content :: String,
     rating :: Int
} deriving (Generic, Show)

instance FromJSON LoglistQuote

loglistJSON :: IO (Maybe LoglistQuote)
loglistJSON = do
          res <- simpleHttp "https://loglist.net/api/quote/random"
          return (decode res :: Maybe LoglistQuote)

loglistQuote :: String
loglistQuote = content $ (fromJust . unsafePerformIO) loglistJSON
