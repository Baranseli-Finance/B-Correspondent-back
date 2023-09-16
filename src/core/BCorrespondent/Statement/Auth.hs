{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module BCorrespondent.Statement.Auth 
       (checkToken, 
        getInstitutionCreds, 
        insertInstToken, 
        InstitutionCreds (..)
       ) where

import Control.Lens
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode, encode)
import Control.Monad (join)

checkToken :: HS.Statement UUID Bool
checkToken = rmap (fromMaybe False) $ [maybeStatement| select is_valid :: bool from auth.jwt where id = $1 :: uuid|]

data InstitutionCreds = 
     InstitutionCreds 
     { institutionCredsIdent :: Int64, 
       institutionCredsJwt :: Maybe T.Text,
       institutionCredsIsValid :: Maybe Bool
     }
     deriving stock (Generic)
     deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor InstitutionCreds)]]
          InstitutionCreds

getInstitutionCreds :: HS.Statement T.Text (Maybe InstitutionCreds)
getInstitutionCreds =
  rmap (join . fmap (decode @InstitutionCreds . encode))
  [maybeStatement|
    select 
      jsonb_build_object (
        'ident', i.id :: int8,
        'jwt', jwt.value :: text?,
        'is_valid', jwt.is_valid :: bool?) :: jsonb 
    from auth.institution as i
    left join auth.institution_jwt as inst_jwt
    on i.id = inst_jwt.inst_id
    left join auth.jwt as jwt
    on inst_jwt.jwt_id = jwt.id
    where key = $1 :: text
    order by jwt.created_at desc limit 1|]

insertInstToken :: HS.Statement (Int64, T.Text, UUID) Bool
insertInstToken = 
  rmap (> 0)
  [rowsAffectedStatement|
    with jwt as (
      insert into auth.jwt 
      (id, value)
      values ($3 :: uuid, $2 :: text)
      returning id :: uuid)
    insert into auth.institution_jwt
    (inst_id, jwt_id)
    select $1 :: int8, id from jwt|]