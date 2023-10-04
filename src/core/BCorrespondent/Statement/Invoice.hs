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
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module BCorrespondent.Statement.Invoice 
       ( register, 
         getInvoicesToBeSent, 
         insertFailedInvoices, 
         updateStatus,
         assignTextualIdent,
         Status (..)
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
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Int (Int64)
import Control.Lens
import Data.Coerce (coerce)
import Data.String.Conv (toS)
import qualified Data.Vector.Extended as V
import Data.Tuple.Extended (app1, app2, app3, snocT, app16)
import Database.Transaction (ParamsShow (..))
import Data.Aeson (encode, eitherDecode)
import Data.Bifunctor (second)
import TH.Mk (mkArbitrary)
import GHC.Generics
import Data.Aeson.WithField (WithField)
import qualified Data.Text as T
import Data.Bifunctor (first)
import Data.Tuple (swap)

data Status = 
       Registered
     | ForwardedToPaymentProvider 
     | Confirmed
     | Declined
     | ForwardedToinitiator
  deriving stock (Generic, Show)

instance ParamsShow Status where
  render = show

mkArbitrary ''Status

register :: HS.Statement (Int64, [(InvoiceRegisterRequest, T.Text)]) (Either String [InvoiceRegisterResponse])
register = 
  dimap mkEncoder (sequence . map (eitherDecode @InvoiceRegisterResponse . encode) . V.toList) $ 
  [vectorStatement|
    with
       xs as (
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
          textual_view,
          status)
        select
          $19 :: int8, 
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
          country_code || currency,
          $18 :: text
        from unnest(
          $1 :: text[], 
          $2 :: text[], 
          $3 :: text[],
          $4 :: timestamptz[],
          $5 :: text[], 
          $6 :: text[], 
          $7 :: text?[], 
          $8 :: text?[],
          $9 :: text[], 
          $10 :: text[], 
          $11 :: text?[], 
          $12 :: text?[],
          $13 :: text[],
          $14 :: float8[],
          $15 :: float8[],
          $16 :: text[],
          $17 :: text[])
        as x(
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
            country_code)
        on conflict (customer_id, invoice_id, institution_id) do nothing
        returning id :: int8 as invoice_id, invoice_id as external_id),
      delivery as (
        insert into institution.invoice_to_institution_delivery
        (invoice_id, institution_id)
        select invoice_id, $19 :: int8 from xs),
      external_ident as (  
        insert into institution.invoice_external_ident
        (invoice_id)
        select invoice_id from xs
        returning invoice_id, external_id)
      select  
        jsonb_build_object(
          'externalIdent', external_id, 
          'internalIdent', invoice_id) :: jsonb
      from external_ident|]
  where 
    mkEncoder (instIdent, xs) =
      snocT instIdent $
        snocT (toS (show Registered)) $
          app3 (V.map (toS . show)) $
            app16 (V.map (toS . show)) $
              app2 (V.map coerce) $ 
                app1 (V.map coerce) $
                  V.unzip17 $ 
                    V.fromList $ 
                      map ((uncurry snocT . swap) . first encodeInvoice) xs

assignTextualIdent :: HS.Statement (Int64, [Int64]) ()
assignTextualIdent =
  lmap (second V.fromList)
  [resultlessStatement|
     with 
       max_ident as (
        select 
          max(id) as max_ident
        from institution.invoice 
        where institution_id = $1 :: int8)
     update institution.invoice
     set textual_view = 
           textual_view || 
            (repeat('0', 9 - length(cast(sub.number as text)))
              || cast(sub.number as text))
     from (
       select
        s.number, 
        f.invoice_id
       from
       (select 
          invoice_id,
          rank() over (order by invoice_id asc) as rank
        from unnest($2 :: int8[]) as invoice_id) as f
       inner join
       (select
          number,
          rank() over ( order by number asc) as rank
        from generate_series(
          ((select max_ident from max_ident) - 
           (select count(*) from unnest($2 :: int8[])) + 1),
          (select max_ident from max_ident), 
          1) as number) as s
       on f.rank = s.rank) as sub
       where invoice.id = sub.invoice_id|]

updateStatus :: HS.Statement [(Int64, Status)] ()
updateStatus = 
  lmap (second (V.map (toS . show)) . V.unzip . V.fromList)
  [resultlessStatement|
     update institution.invoice 
     set status = x.status
     from unnest($1 :: int8[], $2 :: text[]) 
     as x(ident, status)
     where id = x.ident|]
  
getInvoicesToBeSent :: HS.Statement () (Either String [WithField "ident" Int64 InvoiceToPaymentProvider])
getInvoicesToBeSent =
  rmap (sequence . map (eitherDecode @(WithField "ident" Int64 InvoiceToPaymentProvider) . encode) .  V.toList)
  [vectorStatement|
    select
      jsonb_build_object(
        'ident', i.id,
        'externalId', d.external_id) :: jsonb
    from institution.invoice as i
    inner join institution.invoice_to_institution_delivery as d
    on i.id = d.invoice_id
    where not d.is_delivered|]

insertFailedInvoices :: HS.Statement [(Int64, T.Text)] ()
insertFailedInvoices =
  lmap (V.unzip . V.fromList)
  [resultlessStatement|
    insert into institution.invoice_to_institution_delivery
    ( invoice_id, 
      institution_id, 
      attempts, 
      last_attempt_sent_at, 
      error)
    select
      x.invoice_id, 
      i.institution_id, 
      1, 
      now(), 
      x.error
    from unnest($1 :: int8[], $2 :: text[]) as x(invoice_id, error)
    inner join institution.invoice as i
    on x.invoice_id = i.id
    on conflict (invoice_id, institution_id)
    do update set
    attempts = invoice_to_institution_delivery.attempts + 1,
    last_attempt_sent_at = now(),
    error = excluded.error|]