{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Statement.Report (fetchDailyInvoices, DailyInvoices (..)) where

import BCorrespondent.Statement.Invoice (Status (..))
import qualified Hasql.Statement as HS
import Data.Aeson (FromJSON, eitherDecode, encode)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Word (Word32)
import Data.Time.Calendar.OrdinalDate (Day)
import Hasql.TH
import Control.Lens (dimap)
import Data.Tuple.Extended (snocT)
import Data.String.Conv (toS)

data DailyInvoices = 
     DailyInvoices
     { dailyInvoicesTotal :: !Word32,
       dailyInvoicesRegistered :: !Word32,
       dailyInvoicesForwarded :: !Word32,
       dailyInvoicesProcessed :: !Word32,
       dailyInvoicesConfirmed :: !Word32,
       dailyInvoicesDeclined :: !Word32
     }
   deriving stock (Generic, Show)
    deriving
      (FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor
               DailyInvoices)]]
      DailyInvoices

fetchDailyInvoices :: HS.Statement Day (Either String DailyInvoices)
fetchDailyInvoices =
  dimap 
  (\x ->
    snocT (toS (show ForwardedToPaymentProvider)) $ 
     snocT (toS (show ProcessedByPaymentProvider)) $ 
       snocT (toS (show Confirmed)) $ 
         snocT (toS (show Declined)) $ 
           (x, toS (show Registered)))
  (eitherDecode @DailyInvoices . encode)
  [singletonStatement|
    select 
      jsonb_build_object(
        'total', count(1),
        'registered', count(1) filter (where status = $2 :: text),
        'forwarded', count(1) filter (where status = $6 :: text),
        'processed', count(1) filter (where status = $5 :: text),
        'confirmed', count(1) filter (where status = $4 :: text),
        'declined', count(1) filter (where status = $3 :: text)
      ) :: jsonb
    from institution.invoice
    where created_at :: date = $1 :: date|]