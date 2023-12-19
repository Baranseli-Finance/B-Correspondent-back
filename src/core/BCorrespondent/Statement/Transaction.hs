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


create :: HS.Statement TransactionFromPaymentProvider (Maybe (Int64, T.Text))
create =
  (lmap (
      snocT (toS @_ @T.Text (show ForwardedToPaymentProvider))
    . snocT (toS @_ @T.Text (show ProcessedByPaymentProvider)) 
    . encodeTransactionFromPaymentProvider))
  [maybeStatement|
    with new_transaction as (
      insert into institution.transaction
      ( invoice_id,
        sender,
        city,
        country,
        sender_bank,
        sender_wire_transfer_agent,
        sender_wire_transfer_agent_code,
        sender_bank_operation_code,
        receiver_bank,
        receiver_wire_transfer_agent_code,
        amount,
        currency,
        correspondent_bank,
        correspondent_bank_wire_transfer_agent_code,
        charges,
        created_at,
        description)
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
        $11 :: float8,
        trim(both '"' from $12 :: text),
        $13 :: text,
        $14 :: text,
        trim(both '"' from $15 :: text),
        $16 :: timestamptz,
        $17 :: text
      from institution.invoice
      where external_id = $1 :: uuid
      on conflict (invoice_id) do nothing
      returning id, invoice_id)
    update institution.invoice
    set status = $18 :: text
    where id = (select invoice_id from new_transaction)
    and status = $19 :: text
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
      check_already as (
        select case when s.invoice_id is not null then 'already' else 'ok' end  as status
        from institution.invoice as f
        left join institution.transaction as s
        on f.id = s.invoice_id
        where f.external_id = $1 :: uuid)
    select to_jsonb(coalesce((select * from check_already), 'notfound') :: text) :: jsonb|]

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