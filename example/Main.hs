{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Hledger.Data as HL
import           Hledger.Data.Lenses
import           Hledger.Import
import           Lens.Micro.Platform

addEuro :: Rule
addEuro = rule ".*" $ do
  zoom (tpostingsL . each . pamountL . mixed . each) $ do
    acommodityL .= "€"
    astyleL . ascommoditysideL .= HL.R

main :: IO ()
main = defaultMain
  [ rule ".*" $ tcommentL .= "bankxy:"
  , addEuro
  , rule "Donation" $
      posting "Account2" . paccountL .= "Income:Bribes"
  ]
