{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BCorrespondent.Statement.BalancedBook 
       (initBalancedBook,
        fetchBalancedBook,
        DayOfWeeksHourly (..),
        TotalOfWeekHourly (..),
        DayOfWeek (..)
       ) where

import BCorrespondent.Transport.Model.Frontend (BalancedBookWallet)
import BCorrespondent.Transport.Model.Invoice (Currency)
import BCorrespondent.Statement.Types (DoY (..))
import qualified Hasql.Statement as HS
import Data.Text (Text)
import Hasql.TH
import Control.Lens (dimap)
import Data.Coerce (coerce)
import Data.Word (Word32)
import Data.Tuple.Extended (app1, app2, app3)
import Data.Int (Int64)
import Data.Aeson (FromJSON, encode, eitherDecode, Value)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

data DayOfWeek = 
     DayOfWeek
     { dayOfWeek :: Int, 
       dayTotal :: Int 
     }
    deriving stock (Generic, Show)
    deriving
      (FromJSON)
      via WithOptions 
         '[FieldLabelModifier '[CamelTo2 "_"]]
      DayOfWeek

data TotalOfWeekHourly =
     TotalOfWeekHourly  
     { currency :: !Currency, 
       currencyTotal :: !Double 
     }
    deriving stock (Generic, Show)
    deriving
      (FromJSON)
      via WithOptions 
         '[FieldLabelModifier '[CamelTo2 "_"]]
      TotalOfWeekHourly

data DayOfWeeksHourly = 
     DayOfWeeksHourly
     { start :: !Int, 
       end :: !Int, 
       days :: ![DayOfWeek], 
       total :: ![TotalOfWeekHourly]
     }
    deriving stock (Generic, Show)
    deriving
      (FromJSON)
      via WithOptions 
         '[FieldLabelModifier '[CamelTo2 "_"]]
      DayOfWeeksHourly

decodeG :: forall a . FromJSON a => Maybe (V.Vector Value) -> Either String [a]
decodeG = fromMaybe (Right []) . fmap (sequence . map (eitherDecode @a . encode) .V.toList)

bookDecoder (title, xs, ys) = do
  xs' <- decodeG @DayOfWeeksHourly xs
  ys' <- decodeG @BalancedBookWallet ys
  return (title, xs', ys')

initBalancedBook :: HS.Statement (DoY, Maybe DoY, DoY, Int64) (Either String (Text, [DayOfWeeksHourly], [BalancedBookWallet]))
initBalancedBook =
  dimap 
  (app1 (fromIntegral @Word32 . coerce) . 
   app2 (fmap (fromIntegral @Word32 . coerce)) .
   app3 (fromIntegral @Word32 . coerce))
  bookDecoder
  [singletonStatement|
    select 
     f.title :: text,
     s.timeline :: jsonb[]?,
     t.balances :: jsonb[]?
    from (
      select
        title 
      from auth.institution 
      where id = $4 :: int8) as f
    left join (
      select
        array_agg(jsonb_build_object(
          'start', f.start,
          'end', f.end,
          'days', f.xs :: jsonb[]?,
          'total', s.ys :: jsonb[]?)) :: jsonb[] as timeline
      from (
        select 
          tbl.start,
          tbl.end,
          array_agg(jsonb_build_object(
            'day_of_week', tbl.day_of_week, 
            'day_total', tbl.total) 
          order by tbl.day_of_week) 
          :: jsonb[] as xs
        from (
          select
            tm.start,
            tm.end,
            i.day_of_week,
            sum(i.count) as total
          from (
            select 
              el as start,
              el + 1 as end
            from generate_series(0, 23, 1) as el) as tm
          cross join (
            select
              count(*),
              extract(dow from appearance_on_timeline) as day_of_week,
              extract(hour from appearance_on_timeline) as start_point,
              extract(hour from appearance_on_timeline) + 1 as end_point
            from institution.invoice
            where extract(doy from appearance_on_timeline) = $3 :: int
            and institution_id = $4 :: int8
            group by day_of_week, start_point, end_point
            union
            select
              count(*),
              extract(dow from appearance_on_timeline) as day_of_week,
              extract(hour from appearance_on_timeline) as start_point,
              extract(hour from appearance_on_timeline) + 1 as end_point
            from mv.invoice_and_transaction
            where extract(doy from appearance_on_timeline) >= $1 :: int
            and coalesce(extract(doy from appearance_on_timeline) <= $2 :: int?, false)
            and institution_id = $4 :: int8
            group by day_of_week, start_point, end_point) as i
          where tm.start = i.start_point and tm.end = i.end_point
          group by tm.start, tm.end, i.day_of_week) as tbl
        group by tbl.start, tbl.end) as f
      inner join (
        select 
          tbl.start,
          tbl.end,
          array_agg(jsonb_build_object(
            'currency', tbl.currency, 
            'currency_total', tbl.total)) :: jsonb[] as ys
        from (
          select
            tm.start,
            tm.end,
            i.currency,
            sum(i.amount) as total
          from (
            select 
              el as start,
              el + 1 as end
           from generate_series(0, 23, 1) as el) as tm
          cross join (
            select
              currency as currency,
              amount,
              extract(hour from appearance_on_timeline) as start,
              extract(hour from appearance_on_timeline) + 1 as end
            from institution.invoice
            where extract(doy from appearance_on_timeline) = $3 :: int
            and institution_id = $4 :: int8
            union 
            select
              invoice_currency as currency,
              invoice_amount as amount,
              extract(hour from appearance_on_timeline) as start,
              extract(hour from appearance_on_timeline) + 1 as end
            from mv.invoice_and_transaction
            where extract(doy from appearance_on_timeline) >= $1 :: int
            and coalesce(extract(doy from appearance_on_timeline) <= $2 :: int?, false)
            and institution_id = $4 :: int8) as i
          where tm.start = i.start and tm.end = i.end
          group by tm.start, tm.end, i.currency) as tbl
      group by tbl.start, tbl.end) as s
    on f.start = s.start and f.end = s.end) as s on true
    left join (
      select
        array_agg(
        jsonb_build_object(
          'currency', currency, 
          'amount', amount,
          'walletType', wallet_type)
        order by wallet_type asc, currency asc)
        as balances
      from institution.wallet
      where institution_id = $4 :: int8) as t on true|]

fetchBalancedBook :: HS.Statement (DoY, DoY, Int64) (Either String (Text, [DayOfWeeksHourly], [BalancedBookWallet]))
fetchBalancedBook =
  dimap 
  (app1 (fromIntegral @Word32 . coerce) .
   app2 (fromIntegral @Word32 . coerce))
  bookDecoder
  [singletonStatement|
    select 
     f.title :: text,
     s.timeline :: jsonb[]?,
     array[] :: jsonb[]?
    from (
      select
        title 
      from auth.institution 
      where id = $3 :: int8) as f
    left join (
      select
        array_agg(jsonb_build_object(
          'start', f.start,
          'end', f.end,
          'days', f.xs :: jsonb[]?,
          'total', s.ys :: jsonb[]?)) :: jsonb[] as timeline
      from (
        select 
          tbl.start,
          tbl.end,
          array_agg(jsonb_build_object(
            'day_of_week', tbl.day_of_week, 
            'day_total', tbl.total) 
          order by tbl.day_of_week) 
          :: jsonb[] as xs
        from (
          select
            tm.start,
            tm.end,
            i.day_of_week,
            sum(i.count) as total
          from (
            select 
              el as start,
              el + 1 as end
            from generate_series(0, 23, 1) as el) as tm
          cross join ( 
            select
              count(*),
              extract(dow from appearance_on_timeline) as day_of_week,
              extract(hour from appearance_on_timeline) as start_point,
              extract(hour from appearance_on_timeline) + 1 as end_point
            from mv.invoice_and_transaction
            where extract(doy from appearance_on_timeline) >= $1 :: int
            and coalesce(extract(doy from appearance_on_timeline) <= $2 :: int, false)
            and institution_id = $3 :: int8
            group by day_of_week, start_point, end_point) as i
          where tm.start = i.start_point and tm.end = i.end_point
          group by tm.start, tm.end, i.day_of_week) as tbl
        group by tbl.start, tbl.end) as f
      inner join (
        select 
          tbl.start,
          tbl.end,
          array_agg(jsonb_build_object(
            'currency', tbl.currency, 
            'currency_total', tbl.total)) :: jsonb[] as ys
        from (
          select
            tm.start,
            tm.end,
            i.currency,
            sum(i.amount) as total
          from (
            select 
              el as start,
              el + 1 as end
           from generate_series(0, 23, 1) as el) as tm
          cross join (
            select
              invoice_currency as currency,
              invoice_amount as amount,
              extract(hour from appearance_on_timeline) as start,
              extract(hour from appearance_on_timeline) + 1 as end
            from mv.invoice_and_transaction
            where extract(doy from appearance_on_timeline) >= $1 :: int
            and coalesce(extract(doy from appearance_on_timeline) <= $2 :: int, false)
            and institution_id = $3 :: int8) as i
          where tm.start = i.start and tm.end = i.end
          group by tm.start, tm.end, i.currency) as tbl
      group by tbl.start, tbl.end) as s
    on f.start = s.start and f.end = s.end) as s on true|]
