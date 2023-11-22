{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}

module Request 
       ( make,
         safeMake, 
         makePostReq,
         makePostReq',
         withError, 
         forConcurrentlyNRetry, 
         retry, 
         HTTP.methodPost,
         HTTP.methodPut
        ) where

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import Data.String.Conv
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (ToJSON, encode, FromJSON, eitherDecodeStrict)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client.MultipartFormData (Part, formDataBody)
import qualified UnliftIO.Async as Async (pooledForConcurrentlyN)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Retry (retrying, constantDelay, limitRetries)
import Data.Bifunctor (first)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch (handle, MonadCatch)
import Network.HTTP.Client (HttpException)
import Control.Monad.Catch (MonadThrow, throwM)

make ::
  (ToJSON a, MonadIO m, MonadThrow m) =>
  T.Text
  -> HTTP.Manager
  -> [HTTP.Header]
  -> HTTP.Method
  -> Either (Maybe a) [Part]
  -> m B.ByteString
make url manager headers method bodye = do
 req_tmp <- liftIO $ HTTP.parseRequest $ T.unpack url
 let req =
      case bodye of 
        Left body ->
          pure $ 
            req_tmp {
              HTTP.method = method, 
              HTTP.requestHeaders = headers,
              HTTP.requestBody = 
                HTTP.RequestBodyLBS $ 
                  fromMaybe mempty $ 
                    fmap encode body
            }
        Right parts -> formDataBody parts req_tmp { HTTP.requestHeaders = headers }
 response <- liftIO $ flip HTTP.httpLbs manager =<< req
 let response_status = HTTP.statusCode $ HTTP.responseStatus response
 let response_body = toS $ HTTP.responseBody response
 if response_status ==
     HTTP.statusCode HTTP.status200 ||
    response_status == 
      HTTP.statusCode HTTP.status202 ||
    response_status == 
      HTTP.statusCode HTTP.status201
 then pure response_body
 else liftIO req >>= \r -> throwM $ HTTP.HttpExceptionRequest r $ HTTP.StatusCodeException (fmap (const ()) response) mempty

withError :: 
  forall a b e. 
  FromJSON a => 
  Either (HTTP.Response BL.ByteString) B.ByteString -> 
  (HTTP.Response BL.ByteString -> Either e b) ->
  (a -> Either e b) ->
  Either e b
withError (Right bs) onError onOk = 
  first (\e -> error $ e <> ", raw bytes: " <> toS bs) (eitherDecodeStrict @a bs) >>= onOk
withError (Left err) onError _ = onError err

forConcurrentlyNRetry 
  :: (MonadUnliftIO m, Traversable t) 
  => Int
  -> Int
  -> Int 
  -> (b -> m Bool) 
  -> t a 
  -> (a -> m b) 
  -> m (t b)
forConcurrentlyNRetry attempts threads delay shouldRetry xs go =
  Async.pooledForConcurrentlyN threads xs $ \x ->
    let retryPolicy = constantDelay (delay * 1_000_000) <> limitRetries attempts
    in retrying retryPolicy (const shouldRetry) (const (go x))

retry :: MonadUnliftIO m => Int -> (a -> m Bool) -> m a -> m a
retry delay shouldRetry go =
  let retryPolicy = constantDelay (delay * 10 ^ 6) <> limitRetries 3
  in retrying retryPolicy (const shouldRetry) (const go)

safeMake ::
  (ToJSON a, MonadCatch m, MonadIO m) =>
  T.Text
  -> HTTP.Manager
  -> [HTTP.Header]
  -> HTTP.Method
  -> Either (Maybe a) [Part]
  -> (HttpException -> m (Either String B.ByteString))
  -> m (Either String B.ByteString)
safeMake url manager headers method bodye onError = onError `handle` fmap Right (make url manager headers method bodye)
{-# inline safeMake #-}

makePostReq ::
  forall a m .
  (ToJSON a, MonadCatch m, MonadIO m) =>
  T.Text
  -> HTTP.Manager
  -> [HTTP.Header]
  -> a
  -> (HttpException -> m (Either String B.ByteString))
  -> m (Either String B.ByteString)
makePostReq url manager headers body = safeMake @a url manager headers HTTP.methodPost (Left (Just body))
{-# inline makePostReq #-}

makePostReq' ::
  forall a m .
  (ToJSON a, a ~ (), MonadCatch m, MonadIO m) =>
  T.Text
  -> HTTP.Manager
  -> (HttpException -> m (Either String B.ByteString))
  -> m (Either String B.ByteString)
makePostReq' url manager = safeMake @a url manager [] HTTP.methodPost (Left Nothing)
{-# inline makePostReq' #-}