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
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module BCorrespondent.Notification (make, Invoice (..)) where

import BCorrespondent.Statement.Institution (insertNotification)
import BCorrespondent.Transport.Model.Invoice (Currency)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, templateDir, ask)
import Database.Transaction (transactionM, statement)
import Control.Concurrent.Lifted (fork)
import Control.Lens ((^.), (<&>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Int (Int64)
import Data.Kind (Constraint)
import Data.Aeson (ToJSON, toJSON, Value (Object))
import Data.Aeson.KeyMap (toHashMap)
import Data.Aeson.Key (toText)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.String.Conv (toS)
import Data.Proxy (Proxy (..))
import qualified Text.EDE as E
import Control.Monad.IO.Class
import Data.String (fromString)
import Katip (logTM, Severity (ErrorS, DebugS))
import Data.Traversable (for)
import Data.Either.Combinators (whenLeft)
import Data.HashMap.Strict (mapKeys)
import BuildInfo (location)

type family Notification (s :: Symbol) b :: Constraint


data Invoice = 
     Invoice
     { newInvoiceTextualIdent :: !Text,
       newInvoiceAmount :: !Double,
       newInvoiceCurrency :: !Currency
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor
               Invoice)]]
      Invoice

type instance Notification "new_invoice_issued" Invoice = ()

make :: forall s a . (KnownSymbol s, Show a, ToJSON a, Notification s a) => Int64 -> [a]-> KatipHandlerM ()
make institution_id xs = void $ fork go
  where
    go = do
           $(logTM) DebugS $ fromString $ $location <> " ede xs ---->  " <> show xs
           dir <- fmap (^. katipEnv . templateDir) ask 
           let file = toS dir <> "/" <> toS (symbolVal (Proxy @s)) <> ".ede"
           parseRes <- liftIO $ E.eitherParseFile file
           res <- for parseRes $ \template -> do
                     let bodyRes = 
                          fmap (map toS) $ 
                            sequence $ 
                              map toJSON xs <&> \(Object obj) -> 
                                E.eitherRender template $ 
                                  mapKeys toText $  
                                    toHashMap obj
                     for bodyRes $ \ys -> do
                      $(logTM) DebugS $ fromString $ $location <> " ede parsing result ---->  " <> show ys
                      hasql <- fmap (^. katipEnv . hasqlDbPool) ask 
                      transactionM hasql $ statement insertNotification (institution_id, ys)
           whenLeft (res) $ \error -> $(logTM) ErrorS $ fromString $ show error