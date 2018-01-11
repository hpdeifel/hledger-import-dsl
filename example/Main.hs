{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Hledger.Data as HL
import           Hledger.Data.Lenses
import           Hledger.Import
import           Lens.Micro.Platform
import qualified Data.Text as T

addEuro :: Rule
addEuro = rule ".*" $ do
  zoom (tpostingsL . each . pamountL . mixed . each) $ do
    acommodityL .= "€"
    astyleL . ascommoditysideL .= HL.R

splitBetween :: T.Text -> T.Text -> TransactionM ()
splitBetween acc1 acc2 = do
  -- divide first posting by 2
  zoom (tpostingsL . ix 1) $ do
    pamountL . mixed . ix 0 . aquantityL //= 2
    paccountL .= acc1

  -- add new posting with same amount
  Just newpost <- preuse (tpostingsL . ix 1)
  tpostingsL %= (++ [newpost & paccountL .~ acc2])


splitLoot :: Rule
splitLoot = rule "Robbery" $ do
  -- remember to never steal an odd amount or fix the splitting code to favor us.
  splitBetween "Income:Job" "Liabilities:Business:Accomplice"

main :: IO ()
main = defaultMain
  [ rule ".*" $ tcommentL .= "bankxy:"
  , addEuro
  , rule "Donation" $
      posting "Account2" . paccountL .= "Income:Bribes"
  , splitLoot
  ]
