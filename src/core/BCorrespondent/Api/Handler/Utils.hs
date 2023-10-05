{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Api.Handler.Utils 
       ( withError, 
         withErrorExt, 
         extractMIMEandExts, 
         withEither,
         withLogEither
       ) where

import Katip.Handler (KatipHandlerM)
import BCorrespondent.Transport.Response
import qualified BCorrespondent.Transport.Error as E
import Control.Lens
import Control.Lens.Iso.Extended
import qualified Data.Text as T
import Network.HTTP.Types.URI (extractPath)
import Network.Mime (defaultMimeLookup, fileNameExtensions)
import qualified Data.ByteString as B
import Data.Bifunctor (second)
import BuildInfo (location)
import Katip (logTM, logStr, Severity(ErrorS))
import Data.Functor (($>))

withErrorExt :: Show e => Either e (a, [E.Error]) -> (a -> r) -> Response r
withErrorExt (Left e) _ = Error Nothing $ asError (show e ^. stext)
withErrorExt (Right (x, ws)) ok = Warnings (ok x) ws

withError :: Show e => Either e a -> (a -> r) -> Response r
withError res = withErrorExt (second (,mempty) res)

extractMIMEandExts :: T.Text -> (B.ByteString, [T.Text])
extractMIMEandExts uri = let path = extractPath (uri^.textbs)^.from textbs in (defaultMimeLookup path, fileNameExtensions path)

withEither :: Show e => Either e a -> (a -> KatipHandlerM (Response b)) -> KatipHandlerM (Response b)
withEither (Left e) _ = pure $ Error Nothing $ asError (show e ^. stext)
withEither (Right val) handler = handler val

withLogEither :: Show e => Either e a -> (a -> KatipHandlerM (Response b)) -> KatipHandlerM (Response b)
withLogEither (Right val) handler = handler val
withLogEither (Left e) _ = ($(logTM) ErrorS $ logStr @T.Text $ $location <> "error ---> " <> T.pack (show e)) $> Error Nothing (asError @T.Text (T.pack (show e)))