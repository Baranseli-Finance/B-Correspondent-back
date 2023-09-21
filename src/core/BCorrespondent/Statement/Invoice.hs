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

module BCorrespondent.Statement.Invoice 
       ( register, 
         getInvoicesToBeSent, 
         insertFailedInvoices, 
         updateStatus,
         Status (..)
       )
       where

import BCorrespondent.Transport.Model.Invoice 
       (InvoiceRegisterRequest, 
        InvoiceId (..), 
        ExternalInvoiceId (..), 
        ExternalCustomerId (..),
        InvoiceToElekse,
        encodeInvoice
       )
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Int (Int64)
import Control.Lens
import Data.Coerce (coerce)
import Data.String.Conv (toS)
import qualified Data.Vector.Extended as V
import Data.Tuple.Extended (app1, app2, app3, snocT)
import Database.Transaction (ParamsShow (..))
import Data.Aeson (encode, eitherDecode)
import Data.Bifunctor (second)
import TH.Mk (mkArbitrary)
import GHC.Generics
import Data.Aeson.WithField (WithField)
import qualified Data.Text as T

data Status = Registered | ForwardedToElekse | Confirmed | Declined
  deriving stock (Generic, Show)

instance ParamsShow Status where
  render = show

mkArbitrary ''Status

register :: HS.Statement (Int64, [InvoiceRegisterRequest]) [InvoiceId]
register = 
  dimap mkEncoder (V.toList . V.map coerce) $ 
  [vectorStatement|
    with xs as (
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
          status)
        select
          $17 :: int8, 
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
          $16 :: text
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
          $15 :: float8[])
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
            vat)
        on conflict (customer_id, invoice_id, institution_id) do nothing
        returning id :: int8 as invoice_id)
    insert into institution.invoice_to_institution_delivery
    (invoice_id, institution_id)
    select invoice_id, $17 :: int8 from xs
    returning invoice_id :: int8|]
  where 
    mkEncoder (instId, xs) = 
      snocT instId $
        snocT (toS (show Registered)) $
          app3 (V.map (toS . show)) $ 
            app2 (V.map coerce) $ 
              app1 (V.map coerce) $
                V.unzip15 $ 
                  V.fromList $ 
                    map encodeInvoice xs

updateStatus :: HS.Statement [(Int64, Status)] ()
updateStatus = 
  lmap (second (V.map (toS . show)) . V.unzip . V.fromList)
  [resultlessStatement|
     update institution.invoice 
     set status = x.status
     from unnest($1 :: int8[], $2 :: text[]) 
     as x(ident, status)
     where id = x.ident|]
  
getInvoicesToBeSent :: HS.Statement () (Either String [WithField "ident" Int64 InvoiceToElekse])
getInvoicesToBeSent =
  rmap (sequence . map (eitherDecode @(WithField "ident" Int64 InvoiceToElekse) . encode) .  V.toList)
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