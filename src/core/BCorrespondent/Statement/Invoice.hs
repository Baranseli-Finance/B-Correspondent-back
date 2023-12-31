{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module BCorrespondent.Statement.Invoice 
       ( register, 
         getInvoicesToBeSent,
         updateStatus,
         setInvoiceInMotion,
         Status (..),
         InvoiceToBeSent,
         RegisteredInvoice,
         QueryCredentials (..)
       )
       where

import BCorrespondent.Transport.Model.Invoice 
       (InvoiceRegisterRequest, 
        InvoiceRegisterResponse, 
        ExternalInvoiceId (..), 
        ExternalCustomerId (..),
        InvoiceToPaymentProvider,
        encodeInvoice
       )
import qualified BCorrespondent.Statement.Delivery as D (TableRef (Invoice))
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Int (Int64, Int32)
import Control.Lens
import Data.Coerce (coerce)
import Data.String.Conv (toS)
import qualified Data.Vector.Extended as V
import Data.Tuple.Extended (app1, app2, app3, snocT, app16)
import Database.Transaction (ParamsShow (..))
import Data.Aeson (encode, eitherDecode, FromJSON, parseJSON, withText, toJSON)
import Data.Bifunctor (second)
import TH.Mk (mkArbitrary)
import GHC.Generics
import Data.Aeson.WithField (WithField)
import qualified Data.Text as T
import Data.Bifunctor (first)
import Data.Tuple (swap)
import Data.Aeson.Generic.DerivingVia
import BuildInfo (location)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)


data Status = 
       Registered
     | ForwardedToPaymentProvider 
     | ProcessedByPaymentProvider
     | Confirmed
     | Declined
  deriving stock (Generic, Show, Read, Ord, Eq)

instance ParamsShow Status where
  render = show

instance FromJSON Status where
  parseJSON = withText ($location <> ":Status") $ pure . read @Status . toS

mkArbitrary ''Status

type RegisteredInvoice = WithField "ident" T.Text (WithField "transactionIdent" T.Text InvoiceRegisterResponse)

register :: HS.Statement (Int64, (InvoiceRegisterRequest, T.Text)) (Maybe (Either String RegisteredInvoice))
register = 
  dimap mkEncoder (fmap (eitherDecode @RegisteredInvoice . encode)) $ 
  [maybeStatement|
    with
       curr_idx as (
         insert into institution.transaction_id_structure 
         (institution_id, country, currency, idx)
         values ($19 :: int8, $17 :: text, trim(both '"' from $3 :: text), 1)
          on conflict (institution_id, country, currency) 
          do update set idx = transaction_id_structure.idx + excluded.idx
          returning idx),
       new_invoice as (
        insert into institution.invoice
        ( institution_id,
          invoice_id,
          customer_id,
          currency,
          invoice_time,
          seller,
          seller_address,
          seller_tax_id,
          seller_phone_number,
          buyer,
          buyer_address,
          buyer_tax_id,
          buyer_phone_number,
          payment_description,
          amount,
          vat,
          fee,
          transaction_textual_ident,
          status)
        values (
          $19 :: int8, 
          $1 :: text, 
          $2 :: text,
          trim(both '"' from $3 :: text),
          $4 :: timestamptz, 
          $5 :: text, 
          $6 :: text, 
          $7 :: text?, 
          $8 :: text?, 
          $9 :: text, 
          $10 :: text, 
          $11 :: text?, 
          $12 :: text?,
          $13 :: text, 
          $14 :: float8,
          $15 :: float8,
          trim(both '"' from $16 :: text),
          (select 
            coalesce(
              abbreviation,
              'I' :: text
              || repeat('0', 2 - length(cast (($19 :: int8) as text)))
              || cast (($19 :: int8) as text)
            )
          from auth.institution where id = $19 :: int8)
          || $17 :: text
          || upper(trim(both '"' from $3 :: text))
          || repeat('0', 9 - length(cast((select * from curr_idx) as text)))
          || cast((select * from curr_idx) as text),
          $18 :: text)
        on conflict (customer_id, invoice_id, institution_id) do nothing
        returning id :: int8 as invoice_id, invoice_id as external_id, transaction_textual_ident),
      external_ident as (
        insert into institution.invoice_external_ident
        (invoice_id)
        select invoice_id from new_invoice
        returning invoice_id, external_id)
      select  
        jsonb_build_object(
          'externalIdent', 
          f.external_id,
          'transactionIdent', 
          s.transaction_textual_ident,
          'ident', s.external_id)
        :: jsonb
      from external_ident as f
      inner join new_invoice as s
      on f.invoice_id = s.invoice_id|]
  where 
    mkEncoder (instIdent, invoice) =
      snocT instIdent $
        snocT (toS (show Registered)) $
          app3 ((toS . encode)) $
            app16 ((toS . encode)) $
              app2 (coerce) $ 
                app1 (coerce) $
                  ((uncurry snocT . swap) . first encodeInvoice) invoice

setInvoiceInMotion :: HS.Statement [Int64] ()
setInvoiceInMotion = 
  lmap (\xs -> (V.fromList xs, toS (show ForwardedToPaymentProvider), toJSON D.Invoice))
  [resultlessStatement|
    with delivered as (
      update institution.invoice 
        set status = $2 :: text,
            appearance_on_timeline = now()
        from unnest($1 :: int8[]) as x(ident)
      where id = x.ident
      returning id as ident)
    insert into delivery
    (table_ref, table_ident, is_delivered)
    select ($3 :: jsonb) #>> '{}', ident, true
    from delivered
    on conflict (table_ref, table_ident)
    do update set is_delivered = true|]

updateStatus :: HS.Statement [(Int64, Status)] ()
updateStatus = 
  lmap (second (V.map (toS . show)) . V.unzip . V.fromList)
  [resultlessStatement|
    update institution.invoice 
    set status = x.status
    from unnest($1 :: int8[], $2 :: text[]) 
    as x(ident, status)
    where id = x.ident|]

type InvoiceToBeSent =
     WithField "external" UUID
     (WithField "ident" Int64 
      (WithField "textualIdent" T.Text 
       InvoiceToPaymentProvider))

data QueryCredentials = 
     QueryCredentials
     { provider :: Int64, 
       login :: T.Text, 
       password :: T.Text 
     }
    deriving stock (Generic)
     deriving
     (FromJSON)
     via WithOptions DefaultOptions
         QueryCredentials

getInvoicesToBeSent :: HS.Statement Int32 (Either String [(Int64, Maybe QueryCredentials, [InvoiceToBeSent])])
getInvoicesToBeSent =
  dimap (\x -> (x, toJSON D.Invoice)) decoder
  [vectorStatement|
    select
      f.id :: int8,
      (select
        jsonb_build_object(
          'provider', coalesce(rf.second_id, rs.first_id),
          'login', qc.login,
          'password', qc.password) as cred
      from auth.institution as i
      left join institution.relation rf
      on i.id = rf.first_id and rf.first_id = f.id
      left join institution.relation rs
      on i.id = rs.second_id and rs.second_id = f.id
      inner join auth.query_credentials as qc
      on rs.first_id = qc.institution_id or rf.second_id = qc.institution_id
      where rf.second_id is not null or rs.first_id is not null) :: jsonb?,
      f.xs :: jsonb[]?
    from (
      select
        f.id :: int8,
        array_agg(
        jsonb_build_object(
          'ident', s.id,
          'seller', s.seller :: text,
          'sellerAddress', s.seller_address :: text,
          'sellerTaxId', s.seller_tax_id :: text?,
          'sellerPhone', s.seller_phone_number :: text?,
          'buyer', s.buyer :: text,
          'buyerAddress', s.buyer_address :: text,
          'buyerTaxId', s.buyer_tax_id :: text?,
          'buyerPhone', s.buyer_phone_number :: text?,
          'description', s.payment_description :: text,
          'transactionExpenses', s.fee :: text,
          'currency', s.currency,
          'amount', s.amount,
          'externalId', s.external_id,
          'textualIdent', s.transaction_textual_ident,
          'external', s.invoice_ext_id))
        :: jsonb[]? as xs
      from auth.institution as f
      inner join (
        select
          i.*,
          iei.external_id as invoice_ext_id
        from institution.invoice as i
        left join public.delivery as d
         on (d.table_ref = ($2 :: jsonb) #>> '{}') and d.table_ident = i.id
        inner join institution.invoice_external_ident as iei
        on iei.invoice_id = i.id
        where not coalesce(d.is_delivered, false) 
        and not coalesce(d.is_stuck, false)
        order by i.id asc
        limit $1 :: int) as s
      on f.id = s.institution_id
      group by f.id) as f|]
  where decoder xs = 
          sequence $ 
            V.toList xs <&> \(ident, cred, ys) -> do  
                ys' <- fromMaybe (Right []) $ 
                  let decodeYs = 
                        sequence . 
                        map (eitherDecode @InvoiceToBeSent . encode) .  
                        V.toList
                  in fmap decodeYs ys
                cred' <- sequence $ fmap (eitherDecode @QueryCredentials . encode) cred
                pure (ident, cred', ys')