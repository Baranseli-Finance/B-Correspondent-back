{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Transaction 
       (getTransactionsToBeSent, 
        insertFailedTransactions,
        insertSentTransactions,
        create
       ) 
       where

import BCorrespondent.Transport.Model.Transaction (TransactionDelivery, TransactionId (..))

import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Data.Text as T
import qualified Data.Vector.Extended as V
import Control.Lens
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.WithField (WithField)
import Data.UUID (UUID)

getTransactionsToBeSent :: HS.Statement () (Either String [WithField "id" UUID TransactionDelivery])
getTransactionsToBeSent =
  rmap (sequence . map (eitherDecode @(WithField "id" UUID TransactionDelivery) . encode) .  V.toList)
  [vectorStatement|
    select
      jsonb_build_object(
        'id', t.id, 
        'senderName', t.sender_name) :: jsonb
    from institution.transaction as t
    inner join institution.transaction_to_institution_delivery as d
    on t.id = d.transaction_id
    where not is_delivered|]

insertFailedTransactions :: HS.Statement [(TransactionId, T.Text)] ()
insertFailedTransactions =
  lmap (V.unzip . V.fromList . map (first coerce))
  [resultlessStatement|
    with values as (
      select
        i.institution_id as inst_ident,
        t.id as trans_ident,
        x.error as e
      from unnest($1 :: uuid[], $2 :: text[]) 
        as x(ident, error)  
      inner join institution.transaction as t
      on t.id = x.ident
      inner join institution.invoice as i
      on t.invoice_id = i.id)
    insert into institution.transaction_to_institution_delivery
    ( transaction_id, 
      institution_id,
      attempts, 
      last_attempt_sent_at, 
      error)
    select trans_ident, inst_ident, 1, now(), e from values as v
    on conflict (transaction_id, institution_id)
    do update set
    attempts = transaction_to_institution_delivery.attempts + 1,
    last_attempt_sent_at = now(),
    error = excluded.error|]

insertSentTransactions :: HS.Statement [UUID] ()
insertSentTransactions =
  lmap V.fromList $
  [resultlessStatement|
    update institution.transaction_to_institution_delivery
    set is_delivered = true
    where transaction_id = any($1 :: uuid[])|]

create :: HS.Statement () ()
create = undefined