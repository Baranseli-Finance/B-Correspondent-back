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
        create,
       ) 
       where

import BCorrespondent.Transport.Model.Transaction 
       (TransactionToInitiator,
        TransactionFromPaymentProvider,
        encodeTransactionFromPaymentProvider)
import BCorrespondent.Statement.Invoice 
       (Status (ProcessedByPaymentProvider, ForwardedToPaymentProvider))
import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Data.Text as T
import qualified Data.Vector.Extended as V
import Control.Lens
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.WithField (WithField)
import Data.UUID (UUID)
import Data.Int (Int64)
import Data.Tuple.Extended (snocT)
import Data.String.Conv (toS)


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
--   "swiftMessage": "...",
--   "swiftMessageExt": "txt"
-- }
create :: HS.Statement (Int64, TransactionFromPaymentProvider) (Maybe (Int64, T.Text))
create = 
  (lmap $ \(x, y) ->
    snocT (toS (show ForwardedToPaymentProvider)) $
      snocT (toS (show ProcessedByPaymentProvider)) $ 
        snocT x $ encodeTransactionFromPaymentProvider y)
  [maybeStatement|
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
        trim(both '"' from $9 :: text),
        $10 :: text,
        $11 :: text,
        $12 :: int8
      from institution.invoice_to_institution_delivery
      where external_id = $1 :: uuid
      on conflict (invoice_id) do nothing
      returning id, invoice_id)
    update institution.invoice 
    set status = $13 :: text
    where id = (select invoice_id from new_transaction)
    and status = $14 :: text
    returning institution_id :: int8, textual_view :: text|]