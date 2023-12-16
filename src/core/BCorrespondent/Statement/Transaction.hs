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
       (create, 
        checkTransaction, 
        fetchAbortedTransaction, 
        TransactionCheck (..)
       ) where

import BCorrespondent.Transport.Model.Transaction 
       (TransactionFromPaymentProvider,
        encodeTransactionFromPaymentProvider)
import BCorrespondent.Statement.Invoice
       (QueryCredentials,
        Status (ProcessedByPaymentProvider, ForwardedToPaymentProvider, Declined))
import BCorrespondent.Statement.Delivery (TableRef (Transaction))
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
import Data.Aeson (FromJSON, eitherDecode, encode, toJSON)
import qualified Data.Vector as V
import Data.Tuple.Extended (app2)


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
      check_existence as (
        select
          case 
            when r then 'ok'
            else 'notfound'
          end as status
        from (
          select exists 
          (select * 
           from institution.invoice_to_institution_delivery 
           where external_id = $1 :: uuid) as r)),
      check_already as (
        select 'already' as status
        from institution.invoice_to_institution_delivery as f
        inner join institution.transaction as s
        on f.invoice_id = s.invoice_id
        where f.external_id = $1 :: uuid)
    select to_jsonb(coalesce((select * from check_already), (select * from check_existence)) :: text) :: jsonb|]

fetchAbortedTransaction :: HS.Statement () [(Int64, Either String (Maybe QueryCredentials), T.Text)]
fetchAbortedTransaction = 
  dimap 
  (const (toS (show Declined), toJSON Transaction))
  (map (app2 (sequence . fmap (eitherDecode @QueryCredentials . encode))) . V.toList)
  [vectorStatement|
    select 
      i.id :: int8,
      s.cred :: jsonb?,
      i.textual_view :: text
    from institution.invoice as i
    inner join institution.transaction as t
    on i.id = t.invoice_id
    left join (
      select
        i.id as source,
        coalesce(rf.second_id, rs.first_id) as sink,
        jsonb_build_object(
          'provider', coalesce(rf.second_id, rs.first_id),
          'login', qc.login,
          'password', qc.password) as cred
      from auth.institution as i
      left join institution.relation rf
      on i.id = rf.first_id
      left join institution.relation rs
      on i.id = rs.second_id
      inner join auth.query_credentials as qc
      on rs.first_id = qc.institution_id or rf.second_id = qc.institution_id
      where rf.second_id is not null or rs.first_id is not null) as s
    on i.institution_id = s.source
    left join public.delivery as d
    on (d.table_ref = ($2 :: jsonb) #>> '{}') and d.table_ident = i.id
    where i.status = $1 :: text and not coalesce(d.is_delivered, false) and not coalesce(d.is_stuck, false)|]