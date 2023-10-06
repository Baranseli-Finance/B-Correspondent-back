{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Transaction 
       (getTransactionsToBeSent, 
        insertFailedTransactions,
        insertSentTransactions,
        create,
       ) 
       where

import BCorrespondent.Transport.Model.Transaction 
       (TransactionToInitiator, 
        TransactionId (..), 
        TransactionFromPaymentProvider,
        encodeTransactionFromPaymentProvider)
import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Data.Text as T
import qualified Data.Vector.Extended as V
import Control.Lens
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.WithField (WithField)
import Data.UUID (UUID)
import Data.Int (Int64)
import Data.Tuple.Extended (snocT)

getTransactionsToBeSent :: HS.Statement () (Either String [WithField "id" UUID TransactionToInitiator])
getTransactionsToBeSent =
  rmap (sequence . map (eitherDecode @(WithField "id" UUID TransactionToInitiator) . encode) .  V.toList)
  [vectorStatement|
    select
      jsonb_build_object(
        'id', t.id, 
        'senderName', t.sender_name) :: jsonb
    from institution.transaction as t
    inner join institution.transaction_to_institution_delivery as d
    on t.id = d.transaction_id
    where not is_delivered|]

insertFailedTransactions :: HS.Statement [(TransactionId, T.Text)] ()
insertFailedTransactions =
  lmap (V.unzip . V.fromList . map (first coerce))
  [resultlessStatement|
    with values as (
      select
        i.institution_id as inst_ident,
        t.id as trans_ident,
        x.error as e
      from unnest($1 :: uuid[], $2 :: text[]) 
        as x(ident, error)  
      inner join institution.transaction as t
      on t.id = x.ident
      inner join institution.invoice as i
      on t.invoice_id = i.id)
    insert into institution.transaction_to_institution_delivery
    ( transaction_id, 
      institution_id,
      attempts, 
      last_attempt_sent_at, 
      error)
    select trans_ident, inst_ident, 1, now(), e from values as v
    on conflict (transaction_id, institution_id)
    do update set
    attempts = transaction_to_institution_delivery.attempts + 1,
    last_attempt_sent_at = now(),
    error = excluded.error|]

insertSentTransactions :: HS.Statement [UUID] ()
insertSentTransactions =
  lmap V.fromList $
  [resultlessStatement|
    update institution.transaction_to_institution_delivery
    set is_delivered = true
    where transaction_id = any($1 :: uuid[])|]

-- { "ident": "579b254b-dd5d-40a6-9377-beb6d3af98a3"
--   "sender": "...",
--   "address": "...",
--   "phoneNumber": "...",
--   "bank": "...",
--   "swfitSepaCode": "...",
--   "bankAccount": "...",
--   "amount": "...",
--   "currency": "usd",
--   "correspondentBank": "...",
--   "swfitSepaCodeCorrespondentBank": "...",
--   "swiftMessage": "..."
-- }
create :: HS.Statement (Int64, TransactionFromPaymentProvider) Bool
create = 
  dimap (\(x, y) -> snocT x $ encodeTransactionFromPaymentProvider y) (>0)
  [rowsAffectedStatement|
    with new_transaction as (  
      insert into institution.transaction
      ( invoice_id,
        sender_name,
        sender_address,
        sender_phone_number,
        sender_bank,
        swift_sepa_code,
        sender_bank_account,
        amount,
        currency,
        correspondent_bank,
      correspondent_bank_swift_sepa_code,
      swift_sepa_message_id)
      select
        invoice_id,
        $2 :: text,
        $3 :: text,
        $4 :: text,
        $5 :: text,
        $6 :: text,
        $7 :: text,
        $8 :: float8,
        $9 :: text,
        $10 :: text,
        $11 :: text,
        $12 :: int8
      from institution.invoice_to_institution_delivery
      where external_id = $1 :: uuid
      returning id, invoice_id)
    insert into institution.transaction_to_institution_delivery 
    (transaction_id, institution_id)
    select
      n.id,
      i.institution_id
    from new_transaction as n
    inner join institution.invoice as i
    on n.invoice_id = i.id|]