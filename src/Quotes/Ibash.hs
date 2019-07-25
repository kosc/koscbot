{-# LANGUAGE OverloadedStrings #-}
module Quotes.Ibash (ibashQuote) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Network.HTTP.Conduit
import Codec.Text.IConv
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.UTF8 (toString)

ibashQuote :: IO String
ibashQuote = do 
  res <- extractQuote
  let doc = parseHtml res
  quote <- runX $ doc >>> css "div.quotbody" /> getText
  return $ unlines quote

getIbashHtml :: IO (Response ByteString)
getIbashHtml = do 
  manager <- newManager tlsManagerSettings
  baseReq <- parseRequest "http://ibash.org.ru/random.php"
  let req = baseReq { requestHeaders = [("User-Agent", "koscbot")] }
  httpLbs req manager

getQuoteBody :: IO ByteString
getQuoteBody = do
  response <- getIbashHtml
  return $ responseBody response

extractQuote :: IO String
extractQuote = do
  res <- getQuoteBody
  let r = fromCP1251 res
  return $ (toString . toStrict) r

fromCP1251 :: ByteString -> ByteString
fromCP1251 = convert "WINDOWS-1251" "UTF-8"
