{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Transaction 
       (createOk,
        createFailure, 
        checkTransaction, 
        fetchForwardedTransaction,
        setPickedForDelivery,
        getForwardedTransactionUUID,
        getTransactionId,
        getTransactionStatus,
        TransactionCheck (..)
       ) where

import BCorrespondent.Transport.Model.Invoice (Currency, Fee)
import BCorrespondent.Transport.Model.Transaction
       (OkTransaction, FailedTransaction, encodeOkTransaction, encodeFailedTransaction)
import BCorrespondent.Statement.Invoice
       (Status (Confirmed, Declined, ForwardedToPaymentProvider, Declined))
import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Data.Text as T
import Control.Lens
import Data.Int (Int64)
import Data.Tuple.Extended (snocT)
import Data.String.Conv (toS)
import Data.UUID (UUID)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, Value (Object), decode)
import qualified Data.Vector as V
import Data.Tuple.Extended (app2)
import Data.Time.Clock (UTCTime)
import Data.Maybe (catMaybes, fromMaybe)
import Database.Transaction (ParamsShow (..))
import qualified Data.Aeson.KeyMap as A
import Control.Monad (join)


createOk :: HS.Statement OkTransaction (Maybe (Int64, Int64, T.Text))
createOk =
  (lmap (
      snocT (toS @_ @T.Text (show ForwardedToPaymentProvider))
    . snocT (toS @_ @T.Text (show Confirmed)) 
    . encodeOkTransaction))
  [maybeStatement|
    with new_transaction as (
      insert into institution.transaction
      ( invoice_id,
        ok_sender,
        ok_city,
        ok_country,
        ok_sender_bank,
        ok_sender_wire_transfer_agent_code,
        ok_sender_bank_operation_code,
        ok_receiver_bank,
        ok_receiver_wire_transfer_agent_code,
        ok_correspondent_bank,
        ok_correspondent_bank_wire_transfer_agent_code,
        ok_transaction_date,
        ok_transaction_time,
        ok_description,
        ok_amount,
        ok_currency,
        ok_fee)
      select
        invoice.id,
        $2 :: text,
        $3 :: text,
        $4 :: text,
        $5 :: text,
        $6 :: text,
        $7 :: text,
        $8 :: text,
        $9 :: text,
        $10 :: text,
        $11 :: text,
        $12 :: date,
        $13 :: time,
        $14 :: text,
        $15 :: float8,
        trim(both '"' from $16 :: text),
        trim(both '"' from $17 :: text)
      from institution.invoice
      where external_id = $1 :: uuid
      on conflict (invoice_id) do nothing
      returning id, invoice_id)
    update institution.invoice
    set status = $18 :: text
    where id = (select invoice_id from new_transaction)
    and status = $19 :: text
    returning id :: int8, institution_id :: int8, transaction_textual_ident :: text|]

createFailure :: HS.Statement FailedTransaction (Maybe (Int64, T.Text))
createFailure =
    (lmap (
      snocT (toS @_ @T.Text (show ForwardedToPaymentProvider))
    . snocT (toS @_ @T.Text (show Declined)) 
    . encodeFailedTransaction))
  [maybeStatement|
    with new_transaction as (
      insert into institution.transaction
      (invoice_id, failure_reason, failure_timestamp)
      select
        invoice.id,
        $2 :: text,
        $3 :: timestamptz
      from institution.invoice
      where external_id = $1 :: uuid
      on conflict (invoice_id) do nothing
      returning id, invoice_id)
    update institution.invoice
    set status = $4 :: text
    where id = (select invoice_id from new_transaction)
    and status = $5 :: text
    returning institution_id :: int8, transaction_textual_ident :: text|]

data TransactionCheck = NotFound | Already | Ok
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via WithOptions
        '[ConstructorTagModifier 
          '[UserDefined ToLower]
        , SumEnc UntaggedVal]
        TransactionCheck

checkTransaction :: HS.Statement UUID (Either String TransactionCheck)
checkTransaction =
  rmap (eitherDecode @TransactionCheck . encode)
  [singletonStatement|
    with 
      check_already as (
        select case when s.invoice_id is not null then 'already' else 'ok' end  as status
        from institution.invoice as f
        left join institution.transaction as s
        on f.id = s.invoice_id
        where f.external_id = $1 :: uuid)
    select to_jsonb(coalesce((select * from check_already), 'notfound') :: text) :: jsonb|]

data TConfirmed = 
     TConfirmed
     { okInvoiceExternalIdent :: UUID,
       okTransactionId :: T.Text,
       okSender :: T.Text,
       okCountry :: T.Text,
       okCity :: T.Text,
       okSenderBank :: T.Text,
       okSenderBankOperationCode :: T.Text,
       okReceiverBank :: T.Text,
       okAmount :: Double,
       okCurrency :: Currency,
       okFee :: Fee,
       okDescription :: T.Text,
       okTimestamp :: UTCTime,
       okStatus :: T.Text
     }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via WithOptions 
        '[ FieldLabelModifier 
          '[UserDefined FirstLetterToLower, 
            UserDefined (StripPrefix "ok")]
         ] TConfirmed    

data TRejected =
     TRejected
     { rejectedExternalIdent :: UUID,
       rejectedTransactionId :: T.Text,
       rejectedReason :: T.Text,
       rejectedTimestamp :: UTCTime,
       rejectedStatus :: T.Text
     }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via WithOptions 
        '[ FieldLabelModifier 
          '[UserDefined FirstLetterToLower,
            UserDefined (StripPrefix "rejected")]
         ] TRejected

data ForwardedTransaction = 
       ForwardedTransactionOk TConfirmed 
     | ForwardedTransactionRejected TRejected
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via WithOptions '[SumEnc UntaggedVal] ForwardedTransaction

instance ParamsShow ForwardedTransaction where
  render = toS . encode

getForwardedTransactionUUID :: Value -> UUID
getForwardedTransactionUUID (Object o) = fromMaybe undefined $ join $ fmap (decode . encode) $ A.lookup "external_ident" o
getForwardedTransactionUUID _ = error "object is expected"

getTransactionId :: Value -> T.Text
getTransactionId (Object o) = T.drop 3 $ fromMaybe undefined $ join $ fmap (decode . encode) $ A.lookup "transaction_id" o
getTransactionId _ = error "object is expected"

getTransactionStatus :: Value -> T.Text
getTransactionStatus (Object o)  = fromMaybe undefined $ join $ fmap (decode . encode) $ A.lookup "status" o
getTransactionStatus _ = error "object is expected"

fetchForwardedTransaction :: HS.Statement () [(Int64, [Value])]
fetchForwardedTransaction =
  dimap
  (const (toS (show Confirmed), toS (show Declined)))  
  (map (app2 (catMaybes . V.toList)) . V.toList)
  [vectorStatement|
    select
      i.id :: int8,
      array_remove(
        array_agg(
          case
            when tr.ok_sender is not null
            then
              jsonb_build_object(
                'external_ident', ext.external_id,
                'transaction_id', inv.transaction_textual_ident,
                'sender', tr.ok_sender,
                'country', tr.ok_country,
                'city', tr.ok_city,
                'sender_bank', tr.ok_sender_bank,
                'sender_bank_operation_code', tr.ok_sender_bank_operation_code,
                'receiver_bank', tr.ok_receiver_bank,
                'amount', tr.ok_amount,
                'currency', tr.ok_currency,
                'fee', tr.ok_fee,
                'description', tr.ok_description,
                'created_at', tr.ok_transaction_date || 'T' || tr.ok_transaction_time || '.00Z',
                'status', 'accepted'
              )
            when tr.failure_reason is not null
            then
              jsonb_build_object(
                'external_ident', ext.external_id,
                'transaction_id', inv.transaction_textual_ident,
                'reason', tr.failure_reason,
                'created_at', 
                to_char(tr.failure_timestamp, 'YYYY-MM-DD') || 'T' || 
                to_char(tr.failure_timestamp, 'HH12:MI:SS.00') || 'Z',
                'status', 'rejected'
              )
            else null
          end), null) :: jsonb?[]    
    from auth.institution as i
    inner join institution.invoice as inv
    on i.id = inv.institution_id
    inner join institution.invoice_external_ident as ext
    on inv.id = ext.invoice_id
    inner join institution.transaction as tr
    on inv.id = tr.invoice_id
    where (inv.status = $1 :: text or inv.status = $2 :: text)
    and not tr.picked_for_delivery group by i.id|]

setPickedForDelivery :: HS.Statement [UUID] ()
setPickedForDelivery =
  lmap V.fromList
  [resultlessStatement|
    update institution.transaction 
    set picked_for_delivery = true
    where invoice_id = any(
      select invoice_id
      from institution.invoice_external_ident 
      where external_id = any($1 :: uuid[]))|]
