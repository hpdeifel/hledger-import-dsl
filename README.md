# hledger-import-dsl

*Warning*: This currently has *hack* status. I don't know if it will
be useful or even work for anything other than my personal use case.

A Haskell library that (somewhat) replaces CSV import rules for
hledger. It provides a simple lensy DSL that somewhat resembles
hledger's own import rules but is more powerful as it embeds arbitrary
Haskell.

## Usage

1. Create a `.rules` file that allows hledger to import the CSV and
   generates transactions which contain all fields that you'd want to
   match on in the description.
   
2. Write a Haskell program using the DSL provided by this library (see
   example below).
   
3. Run your program: `programname CSVFILE`

## Example

```haskell
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
```
