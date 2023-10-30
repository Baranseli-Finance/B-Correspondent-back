{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module BCorrespondent.Notification (make, NewInvoice (..)) where

import Katip.Handler (KatipHandlerM)
import Control.Concurrent.Lifted (fork)
import Control.Monad (void)
import Data.Text (Text)
import Data.Int (Int64)
import Data.Kind (Constraint)
import Data.Aeson (ToJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.String.Conv (toS)
import Data.Proxy (Proxy (..))
import qualified Text.EDE as E
import Control.Monad.IO.Class
import Data.String (fromString)
import Katip (logTM, Severity (ErrorS))
import Data.Traversable (for)
import Data.Either.Combinators (whenLeft)


type family Notification (s :: Symbol) b :: Constraint 


data NewInvoice = NewInvoice { newInvoiceTextualIdent :: Text }
    deriving stock (Generic, Show)
    deriving
      (ToJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor
               NewInvoice)]]
      NewInvoice

type instance Notification "new_invoice_issued" NewInvoice = ()

make :: forall s a . (KnownSymbol s, ToJSON a, Notification s a) => Int64 -> a -> KatipHandlerM ()
make _ _ = void $ fork go
  where
    go = do
           let file = toS (symbolVal (Proxy @s)) <> ".ede"
           parseRes <- liftIO $ E.eitherParseFile file
           res <- for parseRes $ \_ -> undefined
           whenLeft res $ \error -> $(logTM) ErrorS $ fromString $ show error