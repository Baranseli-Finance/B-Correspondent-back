{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module BCorrespondent.Notification (make, Template (..)) where

import Katip.Handler (KatipHandlerM)
import Control.Concurrent.Lifted (fork)
import Control.Monad (void)
import Data.Text (Text)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Data.Kind (Constraint)
import Data.Aeson (ToJSON)

data Template = NewInvoiceIssued

type family Notification a b :: Constraint 

data NewInvoice = NewInvoice { newInvoiceTextualIdent :: Text } 

make :: forall a t . (Typeable t, ToJSON a) => t -> Int64 -> a -> KatipHandlerM ()
make _ _ _ = void $ fork undefined

