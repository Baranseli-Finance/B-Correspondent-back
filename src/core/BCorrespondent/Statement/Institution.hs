{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Institution (initWithdrawal) where

import BCorrespondent.Transport.Model.Institution (Balance, WithdrawalHistoryItem)
import BCorrespondent.Transport.Model.Frontend (WalletType (..))
import qualified Hasql.Statement as HS
import Control.Lens (dimap)
import Hasql.TH
import Data.Int (Int64)
import Data.Aeson (eitherDecode, encode, toJSON, FromJSON, Value)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

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
           'ident', f.id,
           'currency', s.currency,
           'amount', f.amount,
           'withdrawalStatus', f.status,
           'created', f.created_at)) as history
      from institution.withdrawal as f 
      inner join institution.wallet as s
      on s.id = f.wallet_id
      where s.wallet_type = ($2 :: jsonb) #>> '{}'
      group by s.institution_id) as s
    on f.ident = s.ident    
    where f.ident = $1 :: int8|]
  where 
    transform :: forall a . FromJSON a => V.Vector Value -> Either String [a]
    transform = sequence . map (eitherDecode @a . encode) . V.toList 
    decode (xs, ys) = (,) <$> transform @Balance xs <*> fromMaybe (Right []) (fmap (transform @WithdrawalHistoryItem) ys)