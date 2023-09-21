{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Transaction 
       (getTransactionsToBeSent, insertFailedTransactions) 
       where

import BCorrespondent.Transport.Model.Transaction (Transaction, TransactionId)

import qualified Hasql.Statement as HS

getTransactionsToBeSent :: HS.Statement () [Transaction]
getTransactionsToBeSent = undefined

insertFailedTransactions :: HS.Statement [TransactionId] ()
insertFailedTransactions = undefined