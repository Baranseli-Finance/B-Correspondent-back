{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Statement.Auth (checkToken, getInstitutionId, insertInstToken) where

import Control.Lens
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)

checkToken :: HS.Statement UUID Bool
checkToken = rmap (fromMaybe False) $ [maybeStatement| select is_valid :: bool from auth.jwt where id = $1 :: uuid|]

getInstitutionId :: HS.Statement T.Text (Maybe Int64)
getInstitutionId = [maybeStatement|select id :: int8 from auth.institution where key = $1 :: text|]

insertInstToken :: HS.Statement (Int64, T.Text, UUID) Bool
insertInstToken = 
  rmap (> 0)
  [rowsAffectedStatement|
    with jwt as (
      insert into auth.jwt 
      (id, jwt)
      values ($3 :: uuid, $2 :: text)
      returning id :: uuid)
    insert into auth.institution_jwt
    (inst_id, jwt_id)
    select $1 :: int8, id from jwt|]