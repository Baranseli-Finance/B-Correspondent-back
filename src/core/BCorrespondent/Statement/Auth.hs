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
        getUserCredByCode, 
        insertToken,
        insertCode,
        insertResendCode,
        logout,
        InstitutionCreds (..),
        InsertionResult (..),
        AccountType (..),
        CheckToken (..),
        UserCred (..),
        AuthCodeHash (..)
       ) where

import Control.Lens
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.UUID (UUID)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode, encode, eitherDecode')
import Control.Monad (join)
import Data.Bifunctor (first)

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
          '[FieldLabelModifier 
            '[CamelTo2 "_", 
              UserDefined (StripConstructor InstitutionCreds)]]
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
      where id in 
        (select 
            jwt_id
         from auth.user_jwt as uj 
         inner join link as l
         on uj.user_id = l.user_id))  
    update auth.user
    set pass = crypt($1 :: text, gen_salt('md5')),
        modified_at = now()
    where id = (select user_id from link)|]

data InsertionResult = Success T.Text | TMLeft Int64 | User404
    deriving stock (Generic)
    deriving (FromJSON)
    via WithOptions
        '[SumEnc UntaggedVal, 
          ConstructorTagModifier '[CamelTo2 "_"]]
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

data UserCred = UserCred { userCredIdent :: Int64, userCredEmail :: T.Text }
     deriving stock (Generic)
     deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier 
            '[CamelTo2 "_", 
              UserDefined (StripConstructor UserCred)]]
          UserCred

getUserCredByCode :: HS.Statement (Int, T.Text) (Maybe UserCred)
getUserCredByCode =
  dimap (first fromIntegral) (join . fmap (decode @UserCred . encode))
  [maybeStatement|
    with cred as (
      select 
        u.id,
        u.email,
        (c.code = $1 :: int and 
         not c.is_expended and 
         (extract(epoch from c.expire_at) -
          extract(epoch from now()) > 0))
          as is_code_valid
      from auth.user as u
      inner join auth.code as c
      on u.id = c.user_id
      where c.uuid = $2 :: text)
    select 
      jsonb_build_object(
        'ident', id :: int8, 
        'email', email :: text) :: jsonb 
    from cred
    where is_code_valid|]

insertToken :: HS.Statement (Int64, T.Text, UUID, T.Text) Bool
insertToken =
  rmap (> 0)
    [rowsAffectedStatement|
       with
         invalidated_jwt as (
           update auth.jwt 
           set is_valid = false
           where 
             is_valid and
             id in (
              select jwt_id 
              from auth.user_jwt 
              where user_id = $1 :: int8)),
         jwt as (
          insert into auth.jwt
          (value, id)
          select $2 :: text, $3 :: uuid),
         code as (
          update auth.code
          set is_expended = true 
          where uuid = $4 :: text)
       insert into auth.user_jwt
       (user_id, jwt_id)
       values ($1 :: int8, $3 :: uuid)|]

data AccountType = Institution | User
    deriving stock (Generic, Show, Eq)
    deriving (FromJSON)
    via WithOptions
        '[ConstructorTagModifier '[UserDefined ToLower]]
        AccountType

data CheckToken = CheckToken { checkTokenIsValid :: Bool, checkTokenAccountType :: AccountType } 
     deriving stock (Generic)
     deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier 
            '[CamelTo2 "_", 
              UserDefined (StripConstructor CheckToken)]]
          CheckToken

checkToken :: HS.Statement (UUID, Int64) (Maybe CheckToken)
checkToken = 
  rmap (join . fmap (decode @CheckToken . encode))
  [maybeStatement|
   select
     jsonb_build_object (
      'is_valid', is_valid :: bool,
      'account_type', 
      coalesce(uj.ty, ij.ty)) :: jsonb
   from auth.jwt as j
   left join (
    select *, 'user' as ty 
    from auth.user_jwt) as uj
   on j.id = uj.jwt_id 
   and user_id = $2 :: int8
   left join (
    select *, 'institution' as ty 
    from auth.institution_jwt) as ij
   on j.id = ij.jwt_id 
   and inst_id = $2 :: int8
   where id = $1 :: uuid|]

data AuthCodeHash = 
     HashAndCode 
     { hash :: T.Text, 
       code :: Int,
       email :: T.Text
     } 
     | NextAttemptIn Int
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via WithOptions
        '[SumEnc UntaggedVal]
        AuthCodeHash

insertCode :: HS.Statement (T.Text, T.Text, T.Text) (Maybe (Either String AuthCodeHash))
insertCode =
  rmap (fmap (eitherDecode' @AuthCodeHash . encode))
  [maybeStatement|
    with condition as (
      select
        u.id,
        u.email,
        (pass = crypt($2 :: text, pass)) :: bool 
          as is_pass_valid,
        coalesce(
         extract(
         epoch from c.created_at +
         interval '1 min') -
         extract(epoch from now()), 0)
           as time_left
      from auth.user as u
      left join auth.code as c
      on u.id = c.user_id and 
      coalesce(c.browser_fp = $3 :: text, true)
      where u.login = $1 :: text and
      not coalesce(c.is_expended, false)
      order by c.created_at desc limit 1),
    new_code as (
      insert into auth.code 
      (user_id, uuid, browser_fp)
      select id, md5(cast(id as text) || cast(now() as text)), $3 :: text
      from condition
      where (select is_pass_valid from condition) 
      and (select time_left <= 0 from condition)
      returning jsonb_build_object(
        'hash', uuid,
        'code', code, 
        'email', (select email from condition)) as value)
    select to_jsonb(time_left :: int) :: jsonb 
    from condition where time_left > 0
    union
    select value :: jsonb from new_code|]

insertResendCode :: HS.Statement (T.Text, T.Text) (Maybe (Either String AuthCodeHash))
insertResendCode = 
  rmap (fmap (eitherDecode' @AuthCodeHash . encode))
  [maybeStatement|
    with condition as (
      select
        u.id,
        u.email,
        coalesce(
         extract(
         epoch from c.created_at +
         interval '1 min') -
         extract(epoch from now()), 0)
           as time_left
      from auth.user as u
      left join auth.code as c
      on u.id = c.user_id
      where c.uuid = $1 :: text and not c.is_expended),
    new_code as (
      insert into auth.code 
      (user_id, uuid, browser_fp)
      select 
        id, 
        md5(cast(id as text) || cast(now() as text)), 
        $2 :: text
      from condition
      where (select time_left <= 0 from condition)
      returning jsonb_build_object(
        'hash', uuid,
        'code', code, 
        'email', (select email from condition)) as value),
    code as (
      update auth.code
      set is_expended = true
      where uuid = $1 :: text 
      and (select count(*) > 0 from new_code))
    select to_jsonb(time_left :: int) :: jsonb 
    from condition where time_left > 0
    union
    select value :: jsonb from new_code|]

logout :: HS.Statement UUID Bool
logout = rmap (>0) [rowsAffectedStatement|update auth.jwt set is_valid = false where id = $1 :: uuid|]