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
         WithdrawResult (..)
       ) where

import BCorrespondent.Transport.Model.Institution 
       (Balance, WithdrawalHistory, WithdrawalStatus (Registered))
import BCorrespondent.Transport.Model.Frontend (WalletType (..))
import qualified Hasql.Statement as HS
import Control.Lens (dimap, lmap)
import Hasql.TH
import Data.Int (Int64, Int32)
import Data.Aeson (eitherDecode, encode, toJSON, FromJSON, Value)
import qualified Data.Vector as V
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Tuple.Extended (snocT, app1, app2)
import Data.Text (Text)
import Data.UUID (UUID)


initWithdrawal :: HS.Statement (Int64, Int32) (Either String ([Balance], WithdrawalHistory))
initWithdrawal = 
  dimap (snocT (toJSON Debit)) decode
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
  dimap (snocT (toJSON Debit)) (sequence . fmap (eitherDecode @WithdrawalHistory . encode))
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
  lmap ((,toJSON Debit) . V.fromList)
  [rowsAffectedStatement|
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
    where id = w.wallet_ident|]

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
  lmap (app1 toJSON)
  [resultlessStatement|
    with withdrawal as (
      update institution.withdrawal
      set status = ($1 :: jsonb) #>> '{}'
      where external_id = $3 :: uuid)
    update institution.wallet 
    set modified_at = tbl.tm,
        amount = tbl.amount
    from (
      select
        s.id as ident,
        case
          when $2 :: bool 
          then s.amount - f.amount
          else s.amount
        end as amount,
        case 
          when $2 :: bool
          then now()
          else null
        end as tm      
      from institution.withdrawal as f
      inner join institution.wallet as s
      on f.wallet_id = s.id) as tbl
    where id = tbl.ident|]