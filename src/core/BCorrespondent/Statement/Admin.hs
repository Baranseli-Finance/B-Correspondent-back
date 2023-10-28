{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module BCorrespondent.Statement.Admin (insertUser) where

import BCorrespondent.Auth (Role (None))
import BCorrespondent.Transport.Model.Admin (NewUser, encodeNewUser)
import qualified Hasql.Statement as HS
import Control.Lens (lmap)
import Hasql.TH
import Data.Tuple.Extended (snocT)
import Data.String (fromString)

insertUser :: HS.Statement NewUser ()
insertUser = 
  lmap (snocT (fromString (show None)) . encodeNewUser)
  [resultlessStatement|
    with newUser as (
      insert into auth.user
      (login, email, pass)
      values ($1 :: text, $2 :: text, crypt($3 :: text, gen_salt('md5')))
      returning id)
    insert into auth.user_role
    (user_id, role_id)
    select id, (select id from auth.role where role = $4 :: text) from newUser|]