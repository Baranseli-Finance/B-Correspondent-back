{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse (handle) where

import BCorrespondent.Transport.Model.Transaction 
       (TransactionFromPaymentProvider (..))
import BCorrespondent.Statement.Transaction (create)       
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, hasqlDbPool, katipEnv, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Text (Text, pack)
import Data.Int (Int64)
import BuildInfo (location)
import Katip (logTM, logStr, Severity(ErrorS))
import Control.Monad (when)

handle :: TransactionFromPaymentProvider -> KatipHandlerM (Response ())
handle transaction@TransactionFromPaymentProvider 
       {transactionFromPaymentProviderSwiftMessage = swift,
        transactionFromPaymentProviderIdent = uuid} = do 
  resp <- commitSwiftMessage swift
  fmap (const (Ok ())) $
    for_ resp $ \ident -> do
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      isOk <- transactionM hasql $ statement create (ident, transaction)
      let error = pack $ "webhook ended up in failure, ident: " <> show uuid
      when (not isOk) $ $(logTM) ErrorS $ logStr @Text $ $location <> "error ---> " <> error

commitSwiftMessage :: Text -> KatipHandlerM (Either String Int64)
commitSwiftMessage _ = undefined