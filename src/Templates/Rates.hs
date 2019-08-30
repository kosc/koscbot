{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates.Rates (
    mkRates
  , Rate(..)
) where

import Database (getCurrencies)
import Data.Aeson
import Data.Char (toLower)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Templates.Rate (mkRate)

$(do 
  currencies <- runIO getCurrencies
  mkRate $ map (map toLower) currencies)
 
{-
mkJsonInstance :: [String] -> Q [Dec]
mkJsonInstance currencies = (pure.pure) $
  InstanceD
    Nothing
    []
    (ConT $ mkName "FromJSON")
    [FunD (mkName "parseJSON") (InfixE (Just (AppE (VarE withObject) (LitE (StringL "Rate")))) ($) (Just (LamE [VarP $ mkName "v_0"] (InfixE (Just (InfixE (Just (InfixE (Just (ConE Rate)) (VarE <$>) (Just (VarE $ mkName "v_0")))) (VarE .:) (Just (InfixE (Just (LitE (StringL "USD"))) (VarE <*>) (Just (VarE $ mkName "v_0")))))) (VarE .:) (Just (LitE (StringL "RUR")))))))]
-}

mkRates :: [String] -> Q [Dec]
mkRates cryptos = (pure.pure) $
  DataD [] typeName [] Nothing [constr] [DerivClause Nothing [ConT ''Show, ConT ''Generic]]
  where
    typeName = mkName "Rates"
    constr = RecC typeName [(mkName crypto, Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Rate) | crypto <- cryptos]
