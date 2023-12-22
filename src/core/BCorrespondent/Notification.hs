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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module BCorrespondent.Notification 
       ( makeH
       , makeS
       , Invoice (..)
       , Transaction (..)
       , WithdrawalRegister (..)
       , TransactionStatus (..)
       ) where

import BCorrespondent.Statement.Institution (insertNotification)
import BCorrespondent.Transport.Model.Invoice (Currency)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, templateDir, ask)
import Database.Transaction (transactionM, statement)
import BCorrespondent.ServerM (ServerM)
import Control.Concurrent.Lifted (fork)
import Control.Lens ((^.), (<&>))
import Control.Monad (void, join)
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
import Katip (logTM, Severity (ErrorS, DebugS), KatipContext, KatipContextT)
import Data.Traversable (for)
import Data.Either.Combinators (whenLeft)
import Data.HashMap.Strict (mapKeys)
import BuildInfo (location)
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.UUID (UUID)


type family Notification (s :: Symbol) b :: Constraint

data Context m = Context { getTemplateDir :: m Text, getHasql :: m (Pool.Pool Hasql.Connection) }

data Invoice = 
     Invoice
     { invoiceTextualIdent :: !Text,
       invoiceAmount :: !Double,
       invoiceCurrency :: !Currency
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

data Transaction =
     Transaction
     { transactionInvoiceIdent :: !Text,
       transactionIdent :: !UUID
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor
               Transaction)]]
      Transaction

data WithdrawalRegister =
     WithdrawalRegister
     {
       withdrawalRegisterCurrency :: !Currency,
       withdrawalRegisterAmount :: !Double
     } 
    deriving stock (Generic, Show)
    deriving
      (ToJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor
               WithdrawalRegister)]]
      WithdrawalRegister

data TransactionStatus = 
     TransactionStatus
     { transactionStatusIdent :: !Text,
       transactionStatusStatus :: !Text
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor
               TransactionStatus)]]
      TransactionStatus

type instance Notification "new_invoice_issued" Invoice = ()
type instance Notification "invoice_forwarded" Invoice = ()
type instance Notification "transaction_processed" Transaction = ()
type instance Notification "new_withdrawal" WithdrawalRegister = ()
type instance Notification "transaction_status" TransactionStatus = ()

make ::
  forall m s a . 
  (KnownSymbol s, 
   Show a, 
   ToJSON a, 
   Notification s a, 
   KatipContext m, 
   MonadBaseControl IO m
  ) => 
  Context m -> 
  Int64 -> 
  [a] -> 
  m ()
make Context {getTemplateDir, getHasql} institution_id xs = void $ fork go
  where
    go = do
           $(logTM) DebugS $ fromString $ $location <> " ede xs ---->  " <> show xs
           dir <- getTemplateDir 
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
                      hasql <- getHasql
                      transactionM hasql $ statement insertNotification (institution_id, ys)
           whenLeft (join res) $ \error -> $(logTM) ErrorS $ fromString $ "ede error -->  " <> show error

makeH :: forall s a . (KnownSymbol s, Show a, ToJSON a, Notification s a) => Int64 -> [a]-> KatipHandlerM ()
makeH = make @KatipHandlerM @s (Context (fmap (^. katipEnv . templateDir) ask) (fmap (^. katipEnv . hasqlDbPool) ask))
{-# inline makeH #-}

makeS :: forall s a . (KnownSymbol s, Show a, ToJSON a, Notification s a) => Int64 -> [a]-> KatipContextT ServerM ()
makeS = make @(KatipContextT ServerM) @s (Context (fmap (^.templateDir) ask) (fmap (^.hasqlDbPool) ask))
{-# inline makeS #-}