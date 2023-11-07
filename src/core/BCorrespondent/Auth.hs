{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Auth 
       (AuthenticatedUser (..), 
        JWT, 
        UserIdentClaims, 
        generateJWT, 
        validateJwt,
        withAuth, 
        withWSAuth, 
        Auth.AccountType (..),
        Role (..),
        KnownRole
       ) where

import BCorrespondent.Transport.Model.Auth (AuthToken (..))
import BCorrespondent.Transport.Response
import qualified BCorrespondent.Statement.Auth as Auth
import Control.Lens
import Control.Monad (unless, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Time (currentTime)
import qualified Control.Monad.Trans.Except as Except
import qualified Crypto.JOSE as Jose
import Crypto.JWT
import qualified Crypto.JWT as Jose
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson.Generic.DerivingVia
import Data.Bifunctor (first)
import Data.ByteArray (constEq)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Combinators (maybeToRight)
import Data.Int (Int64)
import Data.Swagger (ApiKeyLocation (ApiKeyHeader), ApiKeyParams (..), SecurityScheme (..), SecuritySchemeType (SecuritySchemeApiKey))
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Katip.Handler (KatipHandlerM, katipEnv, jwk, hasqlDbPool, askLoggerIO)
import Network.Wai (Request, requestHeaders)
import Servant.Auth.Server (AuthResult (..), FromJWT (decodeJWT), JWTSettings (..), ToJWT (..), defaultJWTSettings)
import Servant.Auth.Server.Internal.Class (IsAuth (..))
import Servant.Auth.Server.Internal.ConfigTypes (jwtSettingsToJwtValidationSettings)
import Servant.Auth.Server.Internal.Types (AuthCheck (..))
import Servant.Auth.Swagger (HasSecurity (..))
import qualified Network.WebSockets as WS
import Data.Either.Combinators (whenLeft)
import Data.String.Conv (toS)
import Katip
import Control.Lens.Iso.Extended (textbs)
import BuildInfo (location)
import Data.UUID (UUID)
import System.Random (randomIO)
import Database.Transaction (transactionIO, statement)
import Data.Default.Class.Extended (def)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe)

data AuthError = NoAuthHeader | NoBearer | TokenInvalid

data JWT

data Role = 
       Writer 
     | Reader 
     | Admin
     | None
      -- any entity that can initiate an invoice query 
     | Source
  deriving (Eq, Show, Read)

newtype RRole (r :: Role) = UnsafeRRole Role

class KnownRole (r :: Role) where
  roleSing :: RRole r

instance KnownRole 'Reader where
  roleSing = UnsafeRRole Reader

instance KnownRole 'Writer where
  roleSing = UnsafeRRole Writer

instance KnownRole 'Source where
  roleSing = UnsafeRRole Source

instance KnownRole 'Admin where
  roleSing = UnsafeRRole Admin

instance KnownRole 'BCorrespondent.Auth.None where
  roleSing = UnsafeRRole BCorrespondent.Auth.None

roleVal :: forall r proxy. KnownRole r => proxy r -> Role
roleVal _ = 
  case roleSing :: RRole r of
    UnsafeRRole x -> x

instance HasSecurity JWT where
  securityName _ = "JwtSecurity"
  securityScheme _ = SecurityScheme type_ (Just desc)
    where
      type_ = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)
      desc = "JSON Web Token-based API key"

data AuthenticatedUser (r :: Role) = 
     AuthenticatedUser 
     { ident :: !Int64, 
       account :: !Auth.AccountType,
       jwtIdent :: !UUID,
       roles :: !(V.Vector Role),
       institution :: !(Maybe Int64)
     } deriving (Show)

instance FromJWT (AuthenticatedUser r) where
  decodeJWT _ = Right $ AuthenticatedUser 0 Auth.User def (V.singleton BCorrespondent.Auth.None) Nothing

instance ToJWT (AuthenticatedUser r) where
  encodeJWT _ = emptyClaimsSet

instance IsAuth JWT (AuthenticatedUser r) where
  type AuthArgs JWT = '[JWTSettings, (UUID, Int64) -> IO (Maybe Auth.CheckToken)]
  runAuth _ _ = \cfg checkToken -> do
    res <- ask >>= Except.runExceptT . go cfg checkToken
    fromEither res
    where
      go :: JWTSettings -> ((UUID, Int64) -> IO (Maybe Auth.CheckToken)) -> Request -> Except.ExceptT AuthError AuthCheck (AuthenticatedUser r)
      go cfg checkToken req = do
        header <- Except.except $ maybeToRight NoAuthHeader $ lookup "Authorization" (requestHeaders req)
        let bearer = "Token "
        let (mbearer, token) = BS.splitAt (BS.length bearer) header
        unless (mbearer `constEq` bearer) $
          Except.throwE NoBearer
        usere <- liftIO $ validateJwt cfg checkToken token
        Except.except usere
      fromEither (Left NoAuthHeader) = fail "NoAuthHeader"
      fromEither (Left NoBearer) = fail "NoBearer"
      fromEither (Left TokenInvalid) = fail "TokenInvalid"
      fromEither (Right user) = pure user

validateJwt :: forall r. JWTSettings -> ((UUID, Int64) -> IO (Maybe Auth.CheckToken)) -> BS.ByteString -> IO (Either AuthError (AuthenticatedUser r))
validateJwt cfg@JWTSettings {..} checkToken input = do
  keys <- validationKeys
  userClaimSet <- runJOSE @Jose.JWTError $ do
    unverifiedJWT <- Jose.decodeCompact (BSL.fromStrict input)
    Jose.verifyJWT (jwtSettingsToJwtValidationSettings cfg) keys unverifiedJWT
  fmap (join . first (const TokenInvalid)) $
    for userClaimSet $ \UserIdentClaims {userIdentClaimsIdent, userIdentClaimsJwtUUID} -> do 
      res <- checkToken (userIdentClaimsJwtUUID, userIdentClaimsIdent)
      return $ case res of
        Nothing -> Left TokenInvalid
        Just (Auth.CheckToken {..}) ->
          if checkTokenIsValid
          then
            Right $ 
              AuthenticatedUser 
              { ident = userIdentClaimsIdent,
                account = checkTokenAccountType,
                jwtIdent = userIdentClaimsJwtUUID,
                roles = 
                  fromMaybe V.empty $ 
                  fmap (V.map (read . toS)) $
                  checkTokenRole,
                institution = checkTokenInstitution
              }
          else Left TokenInvalid

withAuth :: forall r a.  KnownRole r => AuthResult (AuthenticatedUser r) -> (AuthenticatedUser r -> KatipHandlerM (Response a)) -> KatipHandlerM (Response a)
withAuth (Authenticated user) runApi =
  let res = V.find ((==) (roleVal (Proxy @r))) (roles user)
  in if isJust res 
     then runApi user
     else return $ 
            Error (Just 403) $ 
              asError @T.Text 
                "you are not allowed to perform this action \
                \ or there is no role assigned to you"
withAuth e _ = do
  $(logTM) ErrorS $ logStr @T.Text $ $location <> " ---> auth error:  " <> mkError e
  return $ 
    Error (Just 401) $ 
      asError @T.Text $ 
        "only for authorized personnel, error: " 
        <> mkError e
  where
    mkError BadPassword = "wrong password"
    mkError NoSuchUser = "no user found"
    mkError Indefinite = "an authentication procedure cannot be carried out"

generateJWT :: Jose.JWK -> Int64 -> Maybe Int64 -> Maybe T.Text -> Int64 -> IO (Either Jose.JWTError (BSL.ByteString, UUID))
generateJWT jwk ident institution login expiryIn = do
  t <- currentTime
  uuid <- randomIO @UUID
  let claims =
        Jose.emptyClaimsSet
          & Jose.claimExp ?~ 
            Jose.NumericDate (addUTCTime (fromIntegral expiryIn) t)
          & Jose.claimIat ?~ Jose.NumericDate t
  let user = 
       UserIdentClaims 
       { userIdentClaimsJwtClaims = claims,
         userIdentClaimsIdent = ident, 
         userIdentClaimsLogin = login,
         userIdentClaimsJwtUUID = uuid,
         userIdentClaimsInstitution = institution
       }
  Jose.runJOSE $ do
    alg <- Jose.bestJWSAlg jwk
    fmap ((,uuid) . encodeCompact) $ 
      Jose.signJWT jwk (Jose.newJWSHeader ((), alg)) user

data UserIdentClaims = UserIdentClaims
  { userIdentClaimsJwtClaims :: !Jose.ClaimsSet,
    userIdentClaimsIdent :: !Int64,
    userIdentClaimsLogin :: !(Maybe T.Text),
    userIdentClaimsJwtUUID :: !UUID,
    userIdentClaimsInstitution :: !(Maybe Int64)
  }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[OmitNothingFields 'True,
            FieldLabelModifier 
            '[UserDefined ToLower, 
              UserDefined (StripConstructor UserIdentClaims)]]
          UserIdentClaims

instance Jose.HasClaimsSet UserIdentClaims where
  claimsSet f s = fmap (\a' -> s {userIdentClaimsJwtClaims = a'}) (f (userIdentClaimsJwtClaims s))


withWSAuth :: WS.PendingConnection -> ((AuthenticatedUser r, WS.Connection) -> KatipHandlerM ()) -> KatipHandlerM ()
withWSAuth pend controller = do 
  conn <- liftIO $ WS.acceptRequest pend
  bs <- liftIO $ WS.receiveData @BSL.ByteString conn
  $(logTM) InfoS $ logStr @String $ "ws auth raw data " <> show bs
  let tokenResp = eitherDecode @AuthToken bs
  res <- fmap join $ for tokenResp $ \(AuthToken token) -> do 
    key <- fmap (^. katipEnv . Katip.Handler.jwk) ask
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    auth_logger <- katipAddNamespace (Namespace ["auth"]) askLoggerIO
    let checkToken = transactionIO hasql auth_logger . statement Auth.checkToken
    authRes <- liftIO $ validateJwt (defaultJWTSettings key) checkToken $ token^.textbs
    fmap (first (const "auth error")) $ for authRes $ \auth -> do
      liftIO $ WS.sendDataMessage conn (WS.Text (encode (Ok ())) Nothing)
      controller (auth, conn)
  whenLeft res $ \error -> do
    $(logTM) ErrorS $ logStr $ $location <> " ws closes with an error: " <> error
    let msg = 
          BSL.toStrict $
            encode @(Response ()) $
              Error (Just 401) $
                asError @T.Text $
                  "connection rejected " <> toS error
    liftIO $ pend `WS.rejectRequest` msg