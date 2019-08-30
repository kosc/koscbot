{-# LANGUAGE TemplateHaskell #-}

module Templates.Rate (
    mkRate
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mkRate :: [String] -> Q [Dec]
mkRate currencies = (pure.pure) $
  DataD [] typeName [] Nothing [constr] [DerivClause Nothing [ConT ''Show]]
  where
    typeName = mkName "Rate"
    constr = RecC typeName [(mkName currency, Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Float) | currency <- currencies]
