{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module BCorrespondent.Api.Handler.WS.Utils 
       ( withWS, 
        listenPsql, 
        sendError,
        withResource,
        ListenPsql, 
        Resource (..)
       ) where


import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Transport.Error (asError)
import Katip
import Katip.Handler
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Async.Lifted as Async
import Data.Foldable (for_)
import Data.Either.Combinators (whenLeft)
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Control.Lens
import BuildInfo (location)
import qualified Control.Concurrent.STM.TChan.Lifted as Async
import Control.Concurrent.STM.Lifted (atomically)
import Control.Monad (forever)
import Control.Concurrent.Lifted (fork, killThread)
import qualified Control.Concurrent.MVar.Lifted as Async
import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import qualified Hasql.Notifications as Hasql
import Data.Proxy (Proxy (..))
import Control.Lens.Iso.Extended (bytesLazy)
import Data.String.Conv (toS)
import Control.Concurrent (threadDelay)
import qualified Data.Text.Lazy as TL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Traversable (for)
import Data.String (fromString)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)


data Resource = Transaction | Wallet
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
         Resource

withWS 
  :: forall a .
  FromJSON a =>
  WS.Connection -> 
  (Hasql.Connection -> a -> KatipHandlerM ()) -> 
  KatipHandlerM ()
withWS conn go = do
  $(logTM) DebugS $ logStr @String $ " ws connection established"
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  (db, local) <- liftIO $ Pool.takeResource hasql
   
  ch <- atomically Async.newTChan

  thread <- Async.newEmptyMVar

  -- the first thread is solely responsible for receiving messages from frontend 
  let front = forever $ liftIO (WS.receiveData @BSL.ByteString conn) >>= atomically . Async.writeTChan ch
  -- the second one is for bd
  let back =
        forever $ do
          msg <- atomically $ Async.readTChan ch
          res <- for (eitherDecode @a msg) $ \val -> do
            threadm <- Async.tryTakeMVar thread
            for_ threadm killThread
            forkId <- fork $ go db val
            Async.putMVar thread forkId
          whenLeft res $ \e -> 
            $(logTM) ErrorS $
              fromString $ 
                $location <> 
                " cannot parse " <> 
                toS msg <> ", error " <> e
  let keepAlive = liftIO $ forever $ threadDelay (10 * 10 ^ 6) >> WS.sendPing @TL.Text conn mempty  

  res <- Async.withAsync front $ 
    \front_async ->
       Async.withAsync back $ 
          \back_async ->
             Async.withAsync keepAlive $ 
                \alive_async ->
                   Async.waitAnyCatchCancel 
                     [front_async, back_async, alive_async]

  whenLeft (snd res) $ \error -> do
    threadm <- Async.tryTakeMVar thread
    for_ threadm killThread
    liftIO $ Pool.putResource local db
    $(logTM) ErrorS $ logStr @String $ $location <> " ws ends up with an error ---> " <> show error

type family ListenPsql (s :: Symbol) (b :: Type) :: Constraint

listenPsql :: forall s a b . (KnownSymbol s, ListenPsql s a, FromJSON a, ToJSON b) => WS.Connection -> Hasql.Connection -> Int64 -> (a -> Maybe b) -> IO ()
listenPsql c db userIdent modify = do
  let channel =  toS (symbolVal (Proxy @s)) <> "_" <> toS (show userIdent)
  let channelToListen = Hasql.toPgIdentifier channel
  Hasql.listen db channelToListen
  forever $
    flip Hasql.waitForNotifications db $ 
      \channel payload -> do 
        let resp = eitherDecode @a $ payload^.from bytesLazy
        for_ (sequence (fmap modify resp)) $ \x -> 
          WS.sendDataMessage c (WS.Text (encode (withError x id)) Nothing)

sendError :: WS.Connection -> Text -> IO ()
sendError c msg = WS.sendDataMessage c (WS.Text (encode @(Response ()) (Error Nothing (asError msg))) Nothing)

withResource :: forall r . KnownSymbol r => WS.Connection -> Resource -> IO () -> KatipHandlerM ()
withResource conn resource callback 
  | show resource == toS (symbolVal (Proxy @r)) = liftIO callback
  | otherwise = 
      let err = Error Nothing $ asError @Text "wrong resource" 
      in liftIO $ WS.sendDataMessage conn (WS.Text (encode @(Response ()) err) Nothing)