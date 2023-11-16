module BCorrespondent.Job.Invoice.Query (Query (..)) where

import BCorrespondent.Transport.Model.Invoice (InvoiceToPaymentProvider)
import Network.HTTP.Client (Manager)
import Data.Text (Text)

data Query = Query { query :: Manager -> Text -> Text -> InvoiceToPaymentProvider -> IO (Either String ()) }