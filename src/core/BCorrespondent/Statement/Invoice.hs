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
         insertFailedInvoices, 
         updateStatus,
         getValidation,
         Status (..),
         Validation (..)
       )
       where

import BCorrespondent.Transport.Model.Invoice 
       (InvoiceRegisterRequest, 
        InvoiceRegisterResponse, 
        ExternalInvoiceId (..), 
        ExternalCustomerId (..),
        InvoiceToPaymentProvider,
        Fee,
        Currency,
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
import Data.Aeson (encode, eitherDecode, FromJSON, ToJSON, parseJSON, withText)
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

register :: HS.Statement (Int64, [(InvoiceRegisterRequest, T.Text)]) (Either String [WithField "ident" T.Text (WithField "textualView" T.Text InvoiceRegisterResponse)])
register = 
  dimap mkEncoder (sequence . map (eitherDecode @(WithField "ident" T.Text (WithField "textualView" T.Text InvoiceRegisterResponse)) . encode) . V.toList) $ 
  [vectorStatement|
    with
       series as (
        select 
          array_agg(cast(row_to_json((sub_query))->>'el' as int))
        from (
          select
            generate_series(
            cast(row_to_json((sub_query))->>'v' as int) + 1, 
            (cast(row_to_json((sub_query))->>'v' as int) + 
             array_length($1 :: text[], 1)),
            1) as el
          from (
            select count(id) as v
            from institution.invoice 
            where institution_id = $19 :: int8)
          as sub_query) 
        as sub_query),
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
          trim(both '"' from currency),
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
          trim(both '"' from fee),
          country_code
          || upper(trim(both '"' from currency))
          || repeat('0', 9 - length(cast(current_ident as text)))
          || cast(current_ident as text),
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
          $17 :: text[],
          (select * from series) :: int[])
        as _(
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
            country_code,
            current_ident)
        on conflict (customer_id, invoice_id, institution_id) do nothing
        returning id :: int8 as invoice_id, invoice_id as external_id, textual_view),
      delivery as (
        insert into institution.invoice_to_institution_delivery
        (invoice_id, institution_id, attempts)
        select invoice_id, $19 :: int8, 1 from xs),
      external_ident as (
        insert into institution.invoice_external_ident
        (invoice_id)
        select invoice_id from xs
        returning invoice_id, external_id)
      select  
        jsonb_build_object(
          'externalIdent', f.external_id,
          'textualView', s.textual_view,
          'ident', s.external_id)
        :: jsonb
      from external_ident as f
      inner join xs as s
      on f.invoice_id = s.invoice_id|]
  where 
    mkEncoder (instIdent, xs) =
      snocT instIdent $
        snocT (toS (show Registered)) $
          app3 (V.map (toS . encode)) $
            app16 (V.map (toS . encode)) $
              app2 (V.map coerce) $ 
                app1 (V.map coerce) $
                  V.unzip17 $ 
                    V.fromList $ 
                      map ((uncurry snocT . swap) . first encodeInvoice) xs


updateStatus :: HS.Statement [(Int64, Status)] ()
updateStatus = 
  lmap (second (V.map (toS . show)) . V.unzip . V.fromList)
  [resultlessStatement|
     with invoices as (
       update institution.invoice 
       set status = x.status
       from unnest($1 :: int8[], $2 :: text[]) 
       as x(ident, status)
       where id = x.ident)
     update institution.invoice_to_institution_delivery
     set is_delivered = true 
     where invoice_id = any($1 :: int8[])|]

type InvoiceToBeSent = WithField "ident" Int64 (WithField "textualIdent" T.Text InvoiceToPaymentProvider)

getInvoicesToBeSent :: HS.Statement () (Either String [(Int64, [InvoiceToBeSent])])
getInvoicesToBeSent =
  rmap decoder
  [vectorStatement|
    select
      f.id :: int8,
      array_agg(
      jsonb_build_object(
        'ident', i.id,
        'currency', i.currency,
        'amount', i.amount,
        'externalId', d.external_id,
        'textualIdent', i.textual_view)) 
      :: jsonb[]?
    from auth.institution as f
    left join institution.invoice as i
    on f.id = i.institution_id
    inner join institution.invoice_to_institution_delivery as d
    on i.id = d.invoice_id
    where not d.is_delivered
    group by f.id|]
  where decoder xs = 
          sequence $ 
            V.toList xs <&> \(ident, ys) -> 
              fmap (ident,) $ 
                fromMaybe (Right []) $ 
                  let decodeYs = 
                        sequence . 
                        map (eitherDecode @InvoiceToBeSent . encode) .  
                        V.toList
                  in fmap decodeYs ys


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

data Validation = 
     Validation 
     { validationInvoiceIdent :: Int64,
       validationInvoiceAmount :: Double,
       validationInvoiceCurrency :: Currency,
       validationInvoiceFee :: Fee,
       validationTransactionAmount :: Double,
       validationTransactionCurrency :: Currency 
     }
    deriving stock (Generic)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[FieldLabelModifier
            '[CamelTo2 "_", 
              UserDefined 
              (StripConstructor Validation)]]
          Validation

getValidation :: HS.Statement () (Either String [Validation])
getValidation =
  dimap
  (const (toS (show ProcessedByPaymentProvider)))
  (sequence . V.toList . V.map (eitherDecode @Validation . encode))
  [vectorStatement|
    select
      jsonb_build_object(
        'invoice_ident', i.id,
        'invoice_amount', i.amount,
        'invoice_currency', i.currency,
        'invoice_fee', i.fee,
        'transaction_amount', t.amount,
        'transaction_currency', t.currency
      ) :: jsonb
    from institution.invoice as i
    inner join institution.transaction as t
    on i.id = t.invoice_id
    where i.status = $1 :: text|]