{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Frontend (getCurrentTimeline) where

import BCorrespondent.Statement.Invoice (Status (..))
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, encode, eitherDecode)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Control.Lens (dimap)
import qualified Data.Vector as V
import Data.Tuple.Extended (snocT)
import Data.String.Conv (toS)

data CurrentTimelineValue = 
     CurrentTimelineValue
     { currentTimelineValueStart :: UTCTime,
       currentTimelineValueEnd :: UTCTime,
       currentTimelineValueTextualIdent :: Text,
       currentTimelineValueStatus :: Status
     }
    deriving stock (Generic)
    deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier
            '[CamelTo2 "_", 
              UserDefined 
              (StripConstructor 
               CurrentTimelineValue)]]
          CurrentTimelineValue

getCurrentTimeline :: HS.Statement (UTCTime, UTCTime) (Either String [CurrentTimelineValue])
getCurrentTimeline =
  dimap 
  (snocT (toS (show Declined)) .
   snocT (toS (show Confirmed)) .
   snocT (toS (show ForwardedToPaymentProvider)))
   (sequence . map (eitherDecode @CurrentTimelineValue . encode) . V.toList)
  [singletonStatement|
    select
      tmp.values :: jsonb[]
    from (  
      select
        tm.start,
        tm.end,
        array_agg(
        json_build_object(
        'textual_ident', i.textual_view, 
        'status', i.status)) 
        :: jsonb[] as values
      from (
        select
            el as start,
            el + interval '5min' as end
        from generate_series(
            $1 :: timestamptz,
            $2 :: timestamptz,
            interval '5 min') as _(el)
        where el < $2 :: timestamptz) as tm
      cross join institution.invoice as i
      where 
      (i.status = $3 :: text or 
       i.status = $4 :: text or 
       i.status = $5 :: text) 
      and (i.appearance_on_timeline > tm.start and 
           i.appearance_on_timeline < tm.end)
      group by tm.start, tm.end) as tmp|]