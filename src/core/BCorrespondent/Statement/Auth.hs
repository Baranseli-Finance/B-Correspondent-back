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
        insertNewPassword,
        insertPasswordResetLink,
        getUserIdByEmail, 
        insertToken,
        InstitutionCreds (..),
        InsertionResult (..)
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
import Data.Aeson (FromJSON, decode, encode, eitherDecode')
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

insertNewPassword :: HS.Statement (T.Text, T.Text) Bool
insertNewPassword =
  rmap (> 0)
  [rowsAffectedStatement|
    with link as (
      update auth.password_reset_link
      set is_expended = true
      where link = $2 :: text 
      and now() < valid_until 
      and is_expended is null
      returning user_id),
    jwt as (
      update auth.jwt 
      set is_valid = false 
      where user_id = (select user_id from link))  
    update auth.user
    set pass = crypt($1 :: text, gen_salt('md5')),
        modified = now()
    where id = (select user_id from link)|]

data InsertionResult = Success T.Text | TMLeft Int64 | User404
    deriving stock (Generic)
    deriving (FromJSON)
    via WithOptions
        '[SumEnc UntaggedVal, ConstructorTagModifier '[CamelTo2 "_"]]
        InsertionResult

insertPasswordResetLink :: HS.Statement (Int64, T.Text) (Either String InsertionResult)
insertPasswordResetLink =
  rmap (eitherDecode' @InsertionResult . encode)
  [singletonStatement|
     with 
       tmp as (
         select
           u.id as user_ident, 
           l.created_at, 
           now(),
           u.email
         from auth.user as u 
         left join auth.password_reset_link as l
         on u.id = l.user_id
         where u.id = $1 :: int8 
         and not coalesce(is_expended, false)
         order by l.id desc limit 1),
      link as (
        insert into auth.password_reset_link
        (user_id, link, valid_until)
        select user_ident, $2 :: text, now() + interval '5 min'
        from tmp
        where (select now() > coalesce(created_at, '1970-01-01'::timestamptz) + interval '30 min' from tmp)
        returning (select email from tmp)),
      tm_left as (
        select cast(extract(epoch from created_at + interval '30 min') - extract(epoch from now()) as int) as tm
        from tmp 
        where (select * from link) is null)
     select 
       coalesce(
	       (select to_jsonb(tm :: int8) :: jsonb from tm_left),
         (select to_jsonb(email :: text) :: jsonb from link),
		     to_jsonb('user404' :: text) :: jsonb) :: jsonb|]

getUserIdByEmail :: HS.Statement T.Text (Maybe Int64)
getUserIdByEmail = [maybeStatement|select id :: int8 from auth.user where email = $1 :: text|]

insertToken :: HS.Statement (T.Text, T.Text, T.Text, UUID) Bool
insertToken =
  rmap (> 0)
    [rowsAffectedStatement|
       with
         user_ident as (
           select 
           id,
           (pass = crypt($2 :: text, pass)) :: bool as is_pass_valid 
           from auth.user 
           where email = $1 :: text),
         invalidated_jwt as (
           update auth.jwt 
           set is_valid = false 
           where 
             is_valid and
             id in (select jwt_id from auth.user_jwt where user_id = (select id from user_ident))
             and (select is_pass_valid from user_ident) is true),
         jwt as (
          insert into auth.jwt 
          (value, id)
          select $3 :: text, $4 :: uuid
          where (select is_pass_valid from user_ident))
       insert into auth.user_jwt
       (user_id, jwt_id)
       select id, $4 :: uuid from user_ident
       where (select is_pass_valid from user_ident) is true|]