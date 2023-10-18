{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Institution (getBalances) where

import BCorrespondent.Transport.Model.Institution (Balance)
import BCorrespondent.Transport.Model.Frontend (WalletType (..))
import qualified Hasql.Statement as HS
import Control.Lens (dimap)
import Hasql.TH
import Data.Int (Int64)
import Data.Aeson (eitherDecode, encode, toJSON)
import qualified Data.Vector as V

getBalances :: HS.Statement Int64 (Either String [Balance])
getBalances = 
  dimap 
  (\x -> (x, toJSON Debit))
  (sequence . map (eitherDecode @Balance . encode) . V.toList)
  [vectorStatement|
    select 
      json_build_object(
      'walletIdent', id,
      'currency', currency,
      'amount', amount) :: jsonb
    from institution.wallet 
    where institution_id = $1 :: int8 
    and wallet_type = ($2 :: jsonb) #>> '{}'|]
