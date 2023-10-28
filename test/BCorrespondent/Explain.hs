{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BCorrespondent.Explain (spec_explain) where

import qualified BCorrespondent.Statement.Auth
import qualified BCorrespondent.Statement.Invoice
import qualified BCorrespondent.Statement.Transaction
import qualified BCorrespondent.Statement.Fs
import qualified BCorrespondent.Statement.Dashboard
import qualified BCorrespondent.Statement.History
import qualified BCorrespondent.Statement.Institution
import qualified BCorrespondent.Statement.BalancedBook

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Generics.Product.Positions
import Database.Migration.Test
import GHC.Generics
import Hasql.Decoders
import Hasql.Session
import Hasql.Statement
import Test.Hspec hiding (shouldBe)
import Test.Hspec.DB.Hasql
import Test.Hspec.Expectations.Lifted
import Test.QuickCheck.Extended (Arbitrary (arbitrary), generate)

spec_explain :: Spec
spec_explain =
  describeHasql
    [migrate]
    Nothing
    "explain"
    $ for_ explainTests
    $ \(modl, tests) ->
      context modl $
        for_ tests $
          \(name, ST st) ->
            itHasql name $ do
              let st' =
                    st
                      & position @1 %~ ("explain " <>)
                      & position @3 .~ noResult
              input <- liftIO $ generate arbitrary
              statement input st' >>= (`shouldBe` ())

deriving instance Generic (Statement a b)

-- | Existential wrapper for the query
data ST = forall a b. Arbitrary a => ST (Statement a b)

(==>) a b = (a, b)

(=>>) a b = (a, ST b)

-- | List of all database queries.
explainTests :: [(String, [(String, ST)])]
explainTests =
  [
    "BCorrespondent.Statement.Auth" ==>
     [  "insertInstToken" =>> BCorrespondent.Statement.Auth.insertInstToken,
        "getInstitutionCreds" =>> BCorrespondent.Statement.Auth.getInstitutionCreds,
        "checkToken" =>> BCorrespondent.Statement.Auth.checkToken,
        "insertNewPassword" =>> BCorrespondent.Statement.Auth.insertNewPassword,
        "insertPasswordResetLink" =>> BCorrespondent.Statement.Auth.insertPasswordResetLink,
        "getUserCredByCode" =>> BCorrespondent.Statement.Auth.getUserCredByCode,
        "insertToken" =>> BCorrespondent.Statement.Auth.insertToken,
        "insertCode" =>> BCorrespondent.Statement.Auth.insertCode,
        "insertResendCode" =>> BCorrespondent.Statement.Auth.insertResendCode,
        "logout" =>> BCorrespondent.Statement.Auth.logout
     ]
  , "BCorrespondent.Statement.Invoice" ==>
     [ "register" =>> BCorrespondent.Statement.Invoice.register,
       "getInvoicesToBeSent" =>> BCorrespondent.Statement.Invoice.getInvoicesToBeSent,
       "insertFailedInvoices" =>> BCorrespondent.Statement.Invoice.insertFailedInvoices,
       "updateStatus" =>> BCorrespondent.Statement.Invoice.updateStatus,
       "getValidation" =>> BCorrespondent.Statement.Invoice.getValidation
     ] 
  , "BCorrespondent.Statement.Transaction" ==>
     [  "getTransactionsToBeSent" =>> BCorrespondent.Statement.Transaction.getTransactionsToBeSent,
        "insertFailedTransactions" =>> BCorrespondent.Statement.Transaction.insertFailedTransactions,
        "insertSentTransactions" =>> BCorrespondent.Statement.Transaction.insertSentTransactions,
        "create" =>> BCorrespondent.Statement.Transaction.create
     ]
  , "BCorrespondent.Statement.Fs" ==> 
     [ "insertFiles" =>> BCorrespondent.Statement.Fs.insertFiles]
  , "BCorrespondent.Statement.Dashboard" ==>
     [ "getDashboard" =>> BCorrespondent.Statement.Dashboard.getDashboard,
       "get1HourTimeline" =>> BCorrespondent.Statement.Dashboard.get1HourTimeline,
       "getGap" =>> BCorrespondent.Statement.Dashboard.getGap 
     ]
  , "BCorrespondent.Statement.History" ==>
     ["initTimeline" =>> BCorrespondent.Statement.History.initTimeline,
      "refreshMV" =>> BCorrespondent.Statement.History.refreshMV,
      "getHourShift" =>> BCorrespondent.Statement.History.getHourShift
    ]
  , "BCorrespondent.Statement.Institution" ==>
    [
      "initWithdrawal" =>> BCorrespondent.Statement.Institution.initWithdrawal,
      "registerWithdrawal" =>> BCorrespondent.Statement.Institution.registerWithdrawal,
      "getWithdrawalPage" =>> BCorrespondent.Statement.Institution.getWithdrawalPage,
      "updateWallet" =>> BCorrespondent.Statement.Institution.updateWallet,
      "fetchWithdrawals" =>> BCorrespondent.Statement.Institution.fetchWithdrawals,
      "updateWithdrawalStatus" =>> BCorrespondent.Statement.Institution.updateWithdrawalStatus,
      "modifyWalletAfterWebhook" =>> BCorrespondent.Statement.Institution.modifyWalletAfterWebhook,
      "refreshWalletMV" =>> BCorrespondent.Statement.Institution.refreshWalletMV
    ]
  , "BCorrespondent.Statement.BalancedBook" ==>
    [ "initFirstBalancedBook" =>> BCorrespondent.Statement.BalancedBook.initFirstBalancedBook,
      "initSecondBalancedBook" =>> BCorrespondent.Statement.BalancedBook.initSecondBalancedBook,
      "fetchFirstBalancedBook" =>> BCorrespondent.Statement.BalancedBook.fetchFirstBalancedBook,
      "fetchSecondBalancedBook" =>> BCorrespondent.Statement.BalancedBook.fetchSecondBalancedBook
    ]  
  ]