{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Transaction (handle) where

import BCorrespondent.Statement.Institution (updateWallet)
import qualified BCorrespondent.Statement.Cache as Cache (insert)
import BCorrespondent.Transport.Model.Transaction (OkTransaction (..))
import BCorrespondent.Statement.Transaction (create, checkTransaction)
import qualified BCorrespondent.Statement.Transaction as S (TransactionCheck (..))
import BCorrespondent.Transport.Response (Response (Ok, Error))
import BCorrespondent.Transport.Error (asError)
import Katip.Handler (KatipHandlerM, hasqlDbPool, katipEnv, ask)
import BCorrespondent.Notification (makeH, Transaction (..)) 
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Text (Text, pack)
import BuildInfo (location)
import Katip (logTM, logStr, Severity(ErrorS))
import Control.Monad (void)
import Data.UUID (toText, UUID)
import Data.Default (def)
import Data.Tuple.Extended (del1, sel1)

mkTransactionKey :: UUID -> Text
mkTransactionKey uuid = "transaction" <> toText uuid

handle :: OkTransaction -> KatipHandlerM (Response ())
handle transaction@OkTransaction {okIdent = uuid} = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  checkRes <- transactionM hasql $ statement checkTransaction uuid
  withCheckRes checkRes
  where 
    withCheckRes (Right S.Ok) =
      fmap (const (Ok ())) $ do
        hasql <- fmap (^. katipEnv . hasqlDbPool) ask
        dbRes <- transactionM hasql $ do
          void $ statement Cache.insert (mkTransactionKey uuid, transaction, Just True)
          r <- statement create transaction
          fmap (const (fmap del1 r)) $ for_ (fmap sel1 r) $ statement updateWallet . (:[])
        for_ dbRes $ \(instIdent, textualIdent) ->
          makeH @"transaction_processed" instIdent [Transaction textualIdent uuid]
    withCheckRes (Right S.NotFound) = pure $ Error (Just 404) $ asError @Text $ toText uuid <> " not found"
    withCheckRes (Right S.Already) = pure $ Error (Just 409) $ asError @Text $ toText uuid <> " was expended"
    withCheckRes (Left error) = fmap (const (Error (Just 500) def)) $ $(logTM) ErrorS $ logStr @Text $ $location <> ": error ---> " <> pack error