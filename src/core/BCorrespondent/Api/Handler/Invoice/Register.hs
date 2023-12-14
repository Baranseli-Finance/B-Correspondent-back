{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module BCorrespondent.Api.Handler.Invoice.Register (handle) where

import qualified BCorrespondent.Statement.Invoice as Invoice (register, RegisteredInvoice)
import BCorrespondent.Transport.Response (Response (Error, Warnings))
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Model.Invoice
       (InvoiceRegisterRequest (InvoiceRegisterRequest), 
        InvoiceRegisterResponse, 
        invoiceRegisterRequestCountryISOCode,
        invoiceRegisterRequestInvoiceIdent,
        invoiceRegisterRequestCurrency,
        invoiceRegisterRequestAmount,
        ExternalInvoiceId (..)
       )
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withEither)
import BCorrespondent.Notification (makeH, Invoice (..))
import Katip.Handler
import qualified Data.Text as T
import Control.Lens ((^.), to, (<&>))
import Database.Transaction (statement, transactionM)
import Data.String.Conv (toS)
import Data.Csv (FromRecord, decodeWith, defaultDecodeOptions, HasHeader (NoHeader))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class (liftIO) 
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.List ((\\))
import BuildInfo (location)
import Data.Foldable (for_)
import Data.Aeson.WithField (WithField (..))
import Data.Coerce (coerce)
import Data.Traversable (for)


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
  :: Auth.AuthenticatedUser 'Auth.Source 
  -> [InvoiceRegisterRequest]
  -> KatipHandlerM (Response [WithField "transactionIdent" T.Text InvoiceRegisterResponse])
handle Auth.AuthenticatedUser {..} xs = do
  let decode = decodeWith @CountryCode defaultDecodeOptions NoHeader
  path <- fmap (^. katipEnv . countryCode . to toS) ask
  countryXsE <- fmap decode $ liftIO $ B.readFile path
  withEither countryXsE $ \countryXs -> do 
    let xs' = 
           [ (x, code) |
             x <- xs,
             let val = 
                   flip V.find countryXs $ 
                     \CountryCode {..} -> 
                       countryCodeCountryCode == 
                       invoiceRegisterRequestCountryISOCode x,
             isJust val,
             let msg = error $ $location <> " nothing while resolving CountryCode",
             let code = countryCodeAlpha2 $ fromMaybe msg val   
           ]
    let cmpRes = 
                if length (xs \\ map fst xs') == 0 
                then Right () 
                else let msg = "the following invoices contain invalid country codes: " 
                     in Left $ msg <> show (xs \\ map fst xs')
    res <- withEither cmpRes $ const $ do 
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      fmap (mkResp xs) $ transactionM hasql $ fmap (sequence . catMaybes) $ for xs' $ statement Invoice.register . (ident,)
    
    let response = flip fmap res $ \xs ->  xs <&> \(WithField _ r) -> r
    let notifXxs =
          flip fmap res $ \ys -> 
            ys <&> \(WithField i (WithField text r)) -> 
              [  Invoice text amount currency | 
                InvoiceRegisterRequest
                {invoiceRegisterRequestInvoiceIdent,
                invoiceRegisterRequestAmount = amount,
                invoiceRegisterRequestCurrency = currency} <- xs, 
                i == coerce invoiceRegisterRequestInvoiceIdent
              ]
    fmap (const response) $ for_ res $ const $ for_ notifXxs (makeH @"new_invoice_issued" @Invoice ident . concat)

mkResp :: [InvoiceRegisterRequest] -> Either String [Invoice.RegisteredInvoice] -> Response [Invoice.RegisteredInvoice]
mkResp _ (Left error) = Error Nothing $ asError @T.Text $ toS error
mkResp ys (Right xs) = Warnings xs ws'
  where 
    xs' = xs <&> \(WithField ident _) -> ident
    ws = 
      flip filter ys $ \InvoiceRegisterRequest {invoiceRegisterRequestInvoiceIdent} -> 
        notElem (coerce invoiceRegisterRequestInvoiceIdent) xs'
    ws' = ws <&> \InvoiceRegisterRequest{invoiceRegisterRequestInvoiceIdent} -> 
                    asError @T.Text $ coerce invoiceRegisterRequestInvoiceIdent