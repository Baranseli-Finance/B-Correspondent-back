module BCorrespondent.Job.Invoice.Provider.Elekse (make) where

import BCorrespondent.Transport.Model.Invoice (InvoiceToPaymentProvider)
import BCorrespondent.Job.Invoice.Query (Query (..))
import Data.Text (Text)
import Network.HTTP.Client (Manager)

make :: Query
make = Query { query = go }

go :: Manager -> Text -> Text -> InvoiceToPaymentProvider -> IO (Either String ())
go _ _ _ _ = pure $ Right ()