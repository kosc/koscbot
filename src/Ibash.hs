{-# LANGUAGE OverloadedStrings #-}
module Ibash
    ( ibashQuote
    ) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Network.HTTP.Conduit
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Codec.Text.IConv
import Text.HTML.DOM
import Text.XML (Document)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8 (toString)

ibashQuote = do 
           res <- extractQuote
           let doc = parseHtml res
           quote <- runX $ doc >>> css "div.quotbody" /> getText
           return $ unlines quote

getIbashHtml = do 
               manager <- newManager tlsManagerSettings
               baseReq <- parseRequest "http://ibash.org.ru/random.php"
               let req = baseReq { requestHeaders = [("User-Agent", "koscbot")] }
               httpLbs req manager

getQuoteBody = do
               response <- getIbashHtml
               return $ responseBody response

extractQuote = do
             res <- getQuoteBody
             let r = fromCP1251 res
             return $ (toString . toStrict) r

fromCP1251 = convert ("WINDOWS-1251" :: EncodingName) ("UTF-8" :: EncodingName)
