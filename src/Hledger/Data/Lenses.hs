{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Data.Lenses where

import qualified Hledger.Data.Types as HL
import           Lens.Micro.Platform

import           Hledger.Data.Lenses.Helper

makeLensesL ''HL.Journal

makeLensesL ''HL.Transaction

makeLensesL ''HL.Posting

makeLensesL ''HL.Amount

makeLensesL ''HL.AmountStyle

posting :: HL.AccountName -> Traversal' HL.Transaction HL.Posting
posting name = tpostingsL . each . filtered (\x -> x^.paccountL == name)

mixed :: Lens' HL.MixedAmount [HL.Amount]
mixed f (HL.Mixed as) = fmap HL.Mixed (f as)
