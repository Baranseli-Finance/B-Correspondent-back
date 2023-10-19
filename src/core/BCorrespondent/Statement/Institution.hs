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
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Institution 
       ( initWithdrawal, 
         registerWithdrawal, 
         WithdrawResult (..)
       ) where

import BCorrespondent.Transport.Model.Institution 
       (Balance, WithdrawalHistoryItem, WithdrawalStatus (Registered))
import BCorrespondent.Transport.Model.Frontend (WalletType (..))
import qualified Hasql.Statement as HS
import Control.Lens (dimap)
import Hasql.TH
import Data.Int (Int64)
import Data.Aeson (eitherDecode, encode, toJSON, FromJSON, Value)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Tuple.Extended (snocT)

initWithdrawal :: HS.Statement Int64 (Either String ([Balance], [WithdrawalHistoryItem]))
initWithdrawal = 
  dimap (\x -> (x, toJSON Debit)) decode
  [singletonStatement|
    select
      f.wallets :: jsonb[],
      s.history :: jsonb[]?
    from (
      select
        institution_id as ident,
        array_agg(
          jsonb_build_object(
           'walletIdent', id,
           'currency', currency,
           'amount', amount)) as wallets
      from institution.wallet
      where wallet_type = ($2 :: jsonb) #>> '{}'
      group by institution_id) as f
    left join (
      select 
        s.institution_id as ident,
        array_agg(
          jsonb_build_object(
           'initiator', 
             u.login || '<' || u.email || '>',
           'ident', f.id,
           'currency', s.currency,
           'amount', f.amount,
           'withdrawalStatus', f.status,
           'created', cast(f.created_at as text) || 'Z')
          order by f.created_at desc) as history
      from institution.withdrawal as f 
      inner join institution.wallet as s
      on s.id = f.wallet_id
      inner join auth.user as u
      on f.user_id = u.id
      where s.wallet_type = ($2 :: jsonb) #>> '{}'
      group by s.institution_id) as s
    on f.ident = s.ident    
    where f.ident = $1 :: int8|]
  where 
    transform :: forall a . FromJSON a => V.Vector Value -> Either String [a]
    transform = sequence . map (eitherDecode @a . encode) . V.toList 
    decode (xs, ys) = (,) <$> transform @Balance xs <*> fromMaybe (Right []) (fmap (transform @WithdrawalHistoryItem) ys)

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