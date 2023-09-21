{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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

data Status = Registered | ForwardedToElekse | Confirmed | Declined
  deriving Show

instance ParamsShow Status where
  render = show

register :: HS.Statement (Int64, [InvoiceRegisterRequest]) [InvoiceId]
register = 
  dimap mkEncoder (V.toList . V.map coerce) $ 
  [vectorStatement|
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
    returning id :: int8|]
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
updateStatus = undefined

getInvoicesToBeSent :: HS.Statement () [Int64]
getInvoicesToBeSent = undefined

insertFailedInvoices :: HS.Statement [Int64] ()
insertFailedInvoices = undefined