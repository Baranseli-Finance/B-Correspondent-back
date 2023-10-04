{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module BCorrespondent.Api.Handler.Invoice.Register (handle) where

import qualified BCorrespondent.Statement.Invoice as Invoice (register)
import BCorrespondent.Transport.Response (Response, fromEither)
import BCorrespondent.Transport.Model.Invoice
       (InvoiceRegisterRequest, InvoiceRegisterResponse, invoiceRegisterRequestCountryISOCode)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withEither)
import Katip.Handler
import qualified Data.Text as T
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)
import Data.String.Conv (toS)
import Data.Bifunctor (first)
import Data.Csv (FromRecord, decodeWith, defaultDecodeOptions, HasHeader (NoHeader))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class (liftIO) 
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Data.List ((\\))

data CountryCode = 
     CountryCode
     { countryCodeName :: !T.Text,
       countryCodeAlpha2 :: !T.Text,
       countryCodeAlpha3 :: !T.Text,
       countryCodeCountryCode :: !T.Text,
       countryCodeISO3166 :: !T.Text
     } deriving (Generic, Show)

instance FromRecord CountryCode

handle
  :: Auth.AuthenticatedUser 'Auth.Bank 
  -> [InvoiceRegisterRequest] 
  -> KatipHandlerM (Response [InvoiceRegisterResponse])
handle Auth.AuthenticatedUser {..} xs = do
  let decode = decodeWith @CountryCode defaultDecodeOptions NoHeader
  countryXsE <- fmap decode $ liftIO $ B.readFile "country-code.csv"
  withEither countryXsE $ \countryXs -> do 
    let xs' = 
           [ x |
             x <- xs, 
             isJust $ 
               flip V.find countryXs $ 
                 \CountryCode {..} -> 
                   countryCodeCountryCode == 
                   invoiceRegisterRequestCountryISOCode x
           ]
    let cmpRes = 
                if length (xs \\ xs') == 0 
                then Right () 
                else Left $ "the following invoices contain invalid country code: " <> show (xs \\ xs')
    withEither cmpRes $ const $ do 
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      fmap (fromEither @T.Text . first toS) $ transactionM hasql $ statement Invoice.register (ident, xs)