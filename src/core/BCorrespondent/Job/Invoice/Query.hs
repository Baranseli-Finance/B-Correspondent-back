{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module BCorrespondent.Job.Invoice.Query (Query (..), Response (..)) where

import BCorrespondent.ServerM (ServerM)
import BCorrespondent.Transport.Model.Invoice (InvoiceToPaymentProvider)
import Network.HTTP.Client (Manager)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia
import Data.Aeson (FromJSON)
import Data.Time.Clock (UTCTime)
import Katip (KatipContextT)


data Response = Response { acceptedAt :: UTCTime }
    deriving stock (Generic, Show)
    deriving
      (FromJSON)
      via WithOptions DefaultOptions
      Response

data Query = 
     Query 
     { getAuthToken :: Manager -> Text  -> Text  -> KatipContextT ServerM (Either String Text),  
       query :: Manager -> Text -> InvoiceToPaymentProvider  -> KatipContextT ServerM (Either String Response) 
     }