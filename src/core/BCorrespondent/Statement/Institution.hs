{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Institution 
       ( initWithdrawal, 
         registerWithdrawal,
         getWithdrawalPage,
         updateWallet,
         fetchWithdrawals,
         updateWithdrawalStatus,
         modifyWalletAfterWebhook,
         refreshWalletMV,
         readNotification,
         loadNotification,
         loadUnreadNotification,
         insertNotification,
         WithdrawResult (..)
       ) where

import BCorrespondent.Transport.Model.Institution 
       (Balance, WithdrawalHistory, WithdrawalStatus (Registered))
import BCorrespondent.Transport.Model.Frontend (WalletType (..), Notification, Notifications (..))
import qualified Hasql.Statement as HS
import Control.Lens (dimap, lmap, rmap)
import Hasql.TH
import Data.Int (Int64, Int32)
import Data.Aeson (eitherDecode, encode, toJSON, FromJSON, Value)
import qualified Data.Vector as V
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Tuple.Extended (snocT, app1, app2)
import Data.Text (Text)
import Data.UUID (UUID)
import Hasql.Decoders (noResult)
import Hasql.Encoders (noParams)
import Data.Maybe (fromMaybe)


initWithdrawal :: HS.Statement (Int64, Int32) (Either String ([Balance], WithdrawalHistory))
initWithdrawal = 
  dimap (snocT (toJSON Credit)) decode
  [singletonStatement|
    select
      f.wallets :: jsonb[],
      json_build_object(
        'total', coalesce(s.total, 0) :: int,
        'items', coalesce(s.history, array[] :: jsonb[])) 
      :: jsonb
    from (
      select
        institution_id as ident,
        array_agg(
          jsonb_build_object(
           'walletIdent', id,
           'currency', currency,
           'amount', amount)) as wallets
      from institution.wallet
      where wallet_type = ($3 :: jsonb) #>> '{}'
      group by institution_id) as f
    left join (
      select 
        tmp.institution_id as ident,
        tmp2.total,
        array_agg(
          jsonb_build_object(
           'initiator', tmp.initiator,
           'ident', tmp.id,
           'currency', tmp.currency,
           'amount', tmp.amount,
           'withdrawalStatus', tmp.status,
           'created', cast(tmp.created_at as text) || 'Z')) 
           as history   
      from (
        select 
          s.institution_id,
          u.login || '<' || u.email || '>' as initiator,
          f.id,
          s.currency,
          f.amount,
          f.status,
          f.created_at
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        inner join auth.user as u
        on f.user_id = u.id
        where s.wallet_type = ($3 :: jsonb) #>> '{}'
        order by f.created_at desc
        limit $2 :: int) as tmp
      inner join (
        select
          s.institution_id,
          count(*) as total
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        where s.institution_id = $1 :: int8
        group by s.institution_id) as tmp2
      on tmp.institution_id = tmp2.institution_id 
      group by tmp.institution_id, tmp2.total) as s
    on f.ident = s.ident    
    where f.ident = $1 :: int8|]
  where 
    transform :: forall a . FromJSON a => V.Vector Value -> Either String [a]
    transform = sequence . map (eitherDecode @a . encode) . V.toList
    withHistory = eitherDecode @WithdrawalHistory . encode
    decode (xs, history) = (,) <$> transform @Balance xs <*> withHistory history

data WithdrawResult = NotEnoughFunds | Ok | FrozenFunds Double
     deriving stock (Generic, Show)
     deriving
     (FromJSON)
     via WithOptions
          '[SumEnc UntaggedVal,
            ConstructorTagModifier 
            '[UserDefined ToLower]]
         WithdrawResult

registerWithdrawal :: HS.Statement (Int64, Int64, Double) (Either String WithdrawResult)
registerWithdrawal =
  dimap (snocT (toJSON Registered)) (eitherDecode @WithdrawResult . encode)
  [singletonStatement|
    with
      frozen_funds as (
        select
          f.id,
          f.amount as wallet_amount,
          coalesce(
            sum(s.amount) 
            filter (where s.status = ($4 :: jsonb) #>> '{}'), 
            0.00) as frozen_amount
        from institution.wallet as f
        left join institution.withdrawal as s
        on s.wallet_id = f.id
        where f.id = $2 :: int8
        group by f.id, f.amount),
     withdrawal as (
        insert into institution.withdrawal
        (user_id, wallet_id, amount, status)
        select 
          $1 :: int8, 
          $2 :: int8, 
          $3 :: float8, 
          ($4 :: jsonb) #>> '{}'
        where (
          select
            wallet_amount - frozen_amount - ($3 :: float8) >= 0
          from frozen_funds)
        returning id, wallet_id)   
     select
       case 
         when s.id is null and (f.wallet_amount - $3 :: float8 < 0)
         then to_jsonb('notenoughfunds'::text) 
         when s.id is null and 
              (f.wallet_amount - $3 :: float8 >= 0) and 
              (wallet_amount - frozen_amount - ($3 :: float8) < 0)
         then to_jsonb(f.frozen_amount)
         else to_jsonb('ok'::text)
       end :: jsonb
     from frozen_funds as f
     left join withdrawal s
     on f.id = s.wallet_id|]

getWithdrawalPage :: HS.Statement (Int64, Int32, Int32) (Either String (Maybe WithdrawalHistory))
getWithdrawalPage =
  dimap (snocT (toJSON Credit)) (sequence . fmap (eitherDecode @WithdrawalHistory . encode))
  [maybeStatement|
    select
      json_build_object(
        'total', tbl.total,
        'items', tbl.history :: jsonb[]?) 
      :: jsonb
    from (
      select
        tmp.institution_id as ident,
        tmp2.total,
        array_agg(
          jsonb_build_object(
           'initiator', tmp.initiator,
           'ident', tmp.id,
           'currency', tmp.currency,
           'amount', tmp.amount,
           'withdrawalStatus', tmp.status,
           'created', cast(tmp.created_at as text) || 'Z')) 
           as history   
      from (
        select 
          s.institution_id,
          u.login || '<' || u.email || '>' as initiator,
          f.id,
          s.currency,
          f.amount,
          f.status,
          f.created_at
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        inner join auth.user as u
        on f.user_id = u.id
        where s.wallet_type = ($4 :: jsonb) #>> '{}'
        order by f.created_at desc
        offset (($3 :: int4 - 1) * 10) 
        limit $2 :: int4) as tmp
      inner join (
        select
          s.institution_id,
          count(*) as total
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        where s.institution_id = $1 :: int8
        group by s.institution_id) as tmp2
      on tmp.institution_id = tmp2.institution_id
      where tmp.institution_id = $1 :: int8 
      group by tmp.institution_id, tmp2.total) as tbl|]

updateWallet :: HS.Statement [Int64] Int64
updateWallet =
  lmap ((,toJSON Credit, toJSON Debit) . V.fromList)
  [rowsAffectedStatement|
    with tbl as (
      update institution.wallet
      set amount = w.amount + wallet.amount,
          modified_at = now()
      from (
        select
          w.wallet_ident,
          w.amount,
          w.currency
        from unnest($1 :: int8[]) 
          as list(invoice_ident)
        inner join (
          select
            w.id as wallet_ident,
            inv.id as invoice_ident,
            w.currency,
            tr.amount
          from institution.wallet as w
          inner join auth.institution as i
          on w.institution_id = i.id
          inner join institution.invoice as inv
          on i.id = inv.institution_id
          inner join institution.transaction as tr
          on inv.id = tr.invoice_id
          and w.wallet_type = ($2 :: jsonb) #>> '{}'
          and tr.currency = w.currency) as w
        on list.invoice_ident = w.invoice_ident) as w
      where id = w.wallet_ident
      returning institution_id, w.amount, w.currency)
    update institution.wallet
    set amount = w.amount + wallet.amount,
        modified_at = now()
    from (
      select 
       f.amount,
       w.id
      from (
        select 
          coalesce(rf.second_id, rs.first_id) as ident,
          t.amount,
          t.currency
        from tbl as t
        left join institution.relation as rf
        on rf.first_id = t.institution_id
        left join institution.relation as rs
        on rs.second_id = t.institution_id) as f
      inner join institution.wallet as w
      on w.institution_id = f.ident
      and w.currency = f.currency
      where w.wallet_type = ($3 :: jsonb) #>> '{}') as w
    where wallet.id = w.id|]

fetchWithdrawals :: HS.Statement () [(Int64, Text, Double, UUID)]
fetchWithdrawals =
  dimap (const (toJSON Registered)) V.toList 
  [vectorStatement|
     select
       f.id :: int8,
       s.payment_provider_ident :: text,
       f.amount :: float8,
       f.external_id :: uuid
     from institution.withdrawal as f
     inner join institution.wallet as s
     on f.wallet_id = s.id
     where f.status = ($1 :: jsonb) #>> '{}'|]

updateWithdrawalStatus :: HS.Statement (WithdrawalStatus, [Int64]) ()
updateWithdrawalStatus = 
  lmap (app2 V.fromList . app1 toJSON)
  [resultlessStatement|
    update institution.withdrawal
    set status = ($1 :: jsonb) #>> '{}'
    where id = any($2 :: int8[])|]

modifyWalletAfterWebhook :: HS.Statement (WithdrawalStatus, Bool, UUID) ()
modifyWalletAfterWebhook = 
  lmap (snocT (toJSON Debit) . app1 toJSON)
  [resultlessStatement|
    with withdrawal as (
        update institution.withdrawal
        set status = ($1 :: jsonb) #>> '{}'
        where external_id = $3 :: uuid
        returning amount),
      wallet_data as (
        select 
          institution_id as ident,
          currency
        from institution.wallet as w
        inner join institution.withdrawal as wt
        on w.id = wt.wallet_id
        where external_id = $3 :: uuid)
    update institution.wallet 
    set modified_at = tbl.tm,
        amount = wallet.amount - coalesce(tbl.amount, 0)
    from (
      select
         s.id as ident,
         case
           when $2 :: bool
           then f.amount
           else null
         end as amount,
         case 
           when $2 :: bool
           then now()
           else null
         end as tm
      from institution.withdrawal as f
      inner join institution.wallet as s
      on f.wallet_id = s.id
      where f.external_id = $3 :: uuid
      union 
       select
         w.id as ident,
         case
           when $2 :: bool
           then (select amount from withdrawal)
           else null
         end as amount,
         case 
           when $2 :: bool
           then now()
           else null
         end as tm
       from (
          select   
            coalesce(rf.second_id, rs.first_id) as ident
          from auth.institution as i
          left join institution.relation rf
          on i.id = rf.first_id 
          and rf.first_id = (select ident from wallet_data)
          left join institution.relation rs
          on i.id = rs.second_id 
          and rs.second_id = (select ident from wallet_data)
          where rf.second_id is not null or rs.first_id is not null) as f
       inner join institution.wallet as w
       on w.institution_id = f.ident
       where w.wallet_type = ($4 :: jsonb) #>> '{}'
       and w.currency = (select currency from wallet_data)) as tbl
    where id = tbl.ident|]

refreshWalletMV :: HS.Statement () ()
refreshWalletMV = HS.Statement [uncheckedSql|refresh materialized view mv.wallet|] noParams noResult True

readNotification :: HS.Statement [Int64] ()
readNotification = lmap V.fromList [resultlessStatement|update public.notification set is_read = true where id = any($1 :: int8[]) and not is_read|]

loadNotification :: HS.Statement Int64 (Either String Notifications)
loadNotification = 
  rmap (fmap (Notifications 0) . fromMaybe (Right []) . fmap (sequence . map (eitherDecode @Notification . encode) . V.toList))
  [singletonStatement|
     select
       array_agg(
        jsonb_build_object(
          'ident', id,
          'created', cast(created_at as text) || 'Z',
          'text', body)
        order by created_at desc) 
       :: jsonb[]?
     from public.notification
     where not is_read and user_id = $1 :: int8|]

loadUnreadNotification :: HS.Statement Int64 Int
loadUnreadNotification = 
  rmap fromIntegral 
  [singletonStatement|
    select count(*) :: int 
    from public.notification 
    where not is_read and user_id = $1 :: int8|]

insertNotification :: HS.Statement (Int64, [Text]) ()
insertNotification =
  lmap (app2 V.fromList)
  [resultlessStatement|
    with 
      xs as (
       insert into public.notification 
       (user_id, body)
       select
         f.user_id,
         s.body
       from (
         select user_id 
         from institution.user
         where institution_id = $1 :: int8) as f
         cross join unnest($2 :: text[]) as s(body)
       returning user_id)
    insert into public.notification_counter
    (institution_id, amount)
    select $1 :: int8, (select count(distinct user_id) from xs)|]