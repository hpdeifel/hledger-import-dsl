{-# LANGUAGE LambdaCase #-}

module Hledger.Sed
  ( TransactionM
  , Rule
  , rule
  , matchesRule
  , applyRule
  , applyRules
  , applyRulesToJournal
  , defaultMain
  ) where

import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Hledger.Data as HL
import qualified Hledger.Read as HL
import           Lens.Micro.Platform
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Regex.TDFA

import           Hledger.Data.Lenses

type TransactionM = State HL.Transaction

data Rule = Rule Regex (TransactionM ())

rule :: Text -> TransactionM () -> Rule
rule regex action = Rule (makeRegex (T.unpack regex)) action

matchesRule :: HL.Transaction -> Rule -> Bool
matchesRule t (Rule regex _) = matchTest regex (T.unpack $ HL.tdescription t)

applyRule :: Rule -> HL.Transaction -> HL.Transaction
applyRule (Rule _ action) trans = execState action trans

applyRules :: [Rule] -> HL.Transaction -> HL.Transaction
applyRules rules trans = flip execState trans $
  forM rules $ \r@(Rule _ action) -> do
    trans' <- get
    when (trans' `matchesRule` r) action

applyRulesToJournal :: [Rule] -> HL.Journal -> HL.Journal
applyRulesToJournal rules journal =
  journal & jtxnsL . each %~ applyRules rules

defaultMain :: [Rule] -> IO ()
defaultMain rules = do
  prog <- getProgName
  getArgs >>= \case
    [file] -> do
      HL.readJournalFile (Just "csv") (Just $ file ++ ".rules") False file >>= \case
        Left err -> hPutStrLn stderr err
        Right journal -> do
          let journal' = applyRulesToJournal rules journal
          forM_ (HL.jtxns journal') $ \txn ->
            putStr (HL.showTransactionUnelided txn)
    _ -> do
      hPutStrLn stderr $ "Usage: " ++ prog ++ " CSV-FILE"
      exitFailure
