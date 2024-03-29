{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE NamedFieldPuns #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DataKinds #-}

module BCorrespondent.Job.Transaction (forward) where


import qualified BCorrespondent.Institution.Webhook.Detail.Tochka as Tochka
import BCorrespondent.Statement.Transaction 
       (fetchForwardedTransaction, setPickedForDelivery,
        getForwardedTransactionUUID, getTransactionId, getTransactionStatus,
        ForwardedTransaction (..), TOk (..), TRejected (..))
import qualified BCorrespondent.Statement.Webhook as Webhook
import qualified BCorrespondent.Notification as N
import BCorrespondent.ServerM (ServerM)
import Katip (KatipContextT, logTM, Severity (ErrorS), logStr)
import BuildInfo (location)
import Control.Concurrent.Lifted (threadDelay)
import Control.Monad (forever)
import Data.Text (Text)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.String.Conv (toS)
import Database.Transaction (statement, transactionM)
import Control.Lens ((^.))
import Katip.Handler (hasqlDbPool, ask, rSAKey)
import Data.Bifunctor (bimap, second)
import Data.Either.Combinators (swapEither)
import Control.Arrow ((&&&))
import Data.Int (Int64)
import Data.Aeson (Value, toJSON, encode)
import Data.Default.Class.Extended (def)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64 
import Data.Text.Encoding (decodeUtf8)
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA 
import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Crypto.Hash.Algorithms as RSA


forward :: Int -> Int -> KatipContextT ServerM ()
forward freqBase freq =
  forever $ do
    threadDelay $ freq * freqBase
    hasql <- fmap (^. hasqlDbPool) ask
    key <- fmap (^.rSAKey) ask
    go hasql key
  where 
    go hasql k = do
      dbRes <- transactionM hasql $ do 
        xs <- statement fetchForwardedTransaction ()
        for xs $ \(instId, yse) -> 
          fmap (bimap (instId,) (instId,) . swapEither) $ 
            for yse $ \ys -> do
              statement Webhook.insert (instId, map (mkWebhookMsg k instId) ys)
              fmap (const (map (getTransactionId &&& getTransactionStatus) ys)) $
                statement setPickedForDelivery $ 
                  map getForwardedTransactionUUID ys 
      let (os, es) = partitionEithers dbRes
      for_ es $ \(ident, error) ->
        let msg = 
              $location <> 
              ":forward: transactions forwarding error " <> 
              toS error <>
              ", institution " <> 
              toS (show ident)
        in $(logTM) ErrorS $ logStr @Text msg
      for_ os $ \o -> uncurry (N.makeS @"transaction_status") $ second (map (uncurry N.TransactionStatus)) o

mkWebhookMsg :: RSA.RSAKey -> Int64 -> ForwardedTransaction -> Value 
mkWebhookMsg k 1 x = toJSON $ (Tochka.defRequest (toTochkaMsg (RSA.key k) x)) { Tochka.requestMethod = Tochka.Processed }
mkWebhookMsg _ _ _ = error $ $location <> ":toWebhookMsg: webhook conversion not found"

toTochkaMsg :: RSA.PrivateKey -> ForwardedTransaction -> Tochka.SignedTransaction
toTochkaMsg secret (ForwardedTransactionOk x) =
  let ok = 
        def
        { Tochka.transactionExternalIdent = okExternalIdent x,
          Tochka.transactionTransactionId = okTransactionId x,
          Tochka.transactionCreatedAt = 
            toS $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.00Z" $ okTimestamp x,
          Tochka.transactionStatus = Tochka.Accepted,
          Tochka.transactionSender = Just $ okSender x,
          Tochka.transactionCountry = Just $ okCountry x,
          Tochka.transactionCity = Just $ okCity x,
          Tochka.transactionSenderBank = Just $ okSenderBank x,
          Tochka.transactionSenderBankOperationCode = 
            Just $ okSenderBankOperationCode x,
          Tochka.transactionSenderBankSwiftOrSepaCode = 
            Just $ okSenderBankSwiftOrSepaCode x,
          Tochka.transactionReceiverBank = Just $ okReceiverBank x,
          Tochka.transactionReceiverBankSwiftOrSepaCode = 
            Just $ okReceiverBankSwiftOrSepaCode x,
          Tochka.transactionCorrespondentBank = Just $ okCorrespondentBank x,
          Tochka.transactionCorrespondentBankSwiftOrSepaCode = 
            Just $ okCorrespondentBankSwiftOrSepaCode x,
          Tochka.transactionAmount = Just $ okAmount x,
          Tochka.transactionCurrency = Just $ okCurrency x,
          Tochka.transactionFee = Just $ okFee x,
          Tochka.transactionDescription = Just $ okDescription x
        }
      signature = RSA.sign Nothing (Just RSA.SHA256) secret $ toS @_ @ByteString $ encode ok
      textSignature = decodeUtf8 $ Base64.encode $ toS $ show signature
  in Tochka.SignedTransaction ok textSignature
toTochkaMsg secret (ForwardedTransactionRejected x) =
  let fail = 
          def 
          { Tochka.transactionExternalIdent = rejectedExternalIdent x,
            Tochka.transactionTransactionId = rejectedTransactionId x,
            Tochka.transactionCreatedAt = 
              toS $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.00Z" $ rejectedTimestamp x,
            Tochka.transactionStatus = Tochka.Rejected,
            Tochka.transactionReason = Just $ rejectedError x
          }
      signature = RSA.sign Nothing (Just RSA.SHA256) secret $ toS @_ @ByteString $ encode fail
      textSignature = decodeUtf8 $ Base64.encode $ toS $ show signature
  in Tochka.SignedTransaction fail textSignature