{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module Database.Transaction
  ( transactionIO,
    transactionM,
    statement,
    SessionR,
    ParamsShow (..),
    ask,
    lift,
  )
where

import BCorrespondent.Transport.Payload (Payload (..))
import BCorrespondent.Transport.Error
import Control.Exception (throwIO)
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Generics.Product.Positions
import Data.Generics.Sum.Constructors
import Data.Int
import Data.List
import Data.Pool
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.Vector.Extended as V
import Data.Word
import GHC.Generics
import qualified Hasql.Connection as Hasql
import Hasql.Session (CommandError (..), QueryError (..), ResultError (..), Session)
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql
import Hasql.TH
import Katip
import Katip.Handler
import PostgreSQL.ErrorCodes
import Data.Tuple (Solo (..))
import Data.UUID (UUID)
import Data.Time.Calendar.OrdinalDate (Day)
import Data.Tuple.Extended
import Data.String.Conv (toS)


newtype QueryErrorWrapper = QueryErrorWrapper Hasql.QueryError
  deriving (Show)

instance Exception QueryErrorWrapper

type SessionR a = ReaderT KatipLoggerIO Session a

deriving instance Generic Hasql.QueryError

deriving instance Generic CommandError

deriving instance Generic ResultError

data ViolationError = ForeignKeyVlolation T.Text | UniqueViolation T.Text
  deriving stock Show

instance AsError ViolationError where
  asError (ForeignKeyVlolation e) = asError @T.Text $ "foreign key violation: " <> e
  asError (UniqueViolation e) = asError @T.Text $ "unique key violation: " <> e

instance Exception ViolationError

-- | Run exception-safe database transaction. User action is run inside
-- transaction.
--
-- If user method returns a value then transaction is properly commited,
--
-- If user methods throws an exception then transaction is rolledback,
-- connection is destroyed and exception is rethrown.
--
-- Summary: if this method returns a value, then transaction is commited
-- and connection is returned back to the pool.
--
-- Additional notes:
--
-- Transaction handle takes a Logger as an argument, this logger
-- will be used in all queries inside transaction.
--
-- 'TransactionHandle' is valid only inside transaction scope,
-- using it outside transaction will lead to undefined behavior.
--
-- This method may throw 'SQLException' in case if SQL exception
-- happens during operations that commit or rollback transaction,
-- in this case connection may not be in a clean state.
runSessionIO :: Pool Hasql.Connection -> KatipLoggerIO -> ReaderT KatipLoggerIO Session a -> IO (Either ViolationError a)
runSessionIO pool logger session =
  run >>= either (throwIO . QueryErrorWrapper) pure . join
  where
    run = withResource pool $ \conn ->
      mask $ \release -> do
        bg <- Hasql.run (Hasql.sql [uncheckedSql|begin|]) conn
        let action =
              Hasql.run (runReaderT session logger) conn >>=
                handleDBResult
        let withBegin = do
              result <-
                release action 
                `onException` do
                  logger CriticalS "error while performing sql"
                  Hasql.run (Hasql.sql [uncheckedSql|rollback|]) conn
              cm <- Hasql.run (Hasql.sql [uncheckedSql|commit|]) conn
              traverse (const (pure result)) cm
        traverse (const withBegin) bg

handleDBResult :: Either Hasql.QueryError a -> IO (Either ViolationError a)
handleDBResult (Left e) =
  case codem of
    Just code ->
      if code == foreign_key_violation
        then return $ Left $ ForeignKeyVlolation $ msg <> ", " <> detail
        else
          if code == unique_violation
            then return $ Left $ UniqueViolation $ msg <> ", " <> detail
            else throwIO $ QueryErrorWrapper e
    Nothing -> throwIO $ QueryErrorWrapper e
  where
    codem = e ^? position @3 . _Ctor @"ResultError" . _Ctor @"ServerError" . _1
    msg = maybe mempty toS $ e ^? position @3 . _Ctor @"ResultError" . _Ctor @"ServerError" . _2
    detail = maybe mempty toS $ join $ e ^? position @3 . _Ctor @"ResultError" . _Ctor @"ServerError" . _3
handleDBResult (Right val) = return $ Right val

class ParamsShow a where
  render :: a -> String

instance ParamsShow () where render () = mempty

instance ParamsShow Int where render = show

instance ParamsShow Int32 where render = show

instance ParamsShow Int64 where render = show

instance ParamsShow Word64 where render = show

instance ParamsShow Word32 where render = show

instance ParamsShow Double where render = show

instance ParamsShow B.ByteString where render = B.unpack

instance ParamsShow T.Text where render = T.unpack

instance ParamsShow Day where render = show

instance ParamsShow Bool where
  render True = "true"
  render False = "false" 

instance ParamsShow a => ParamsShow (Solo a) where
  render (Solo x) = render x

instance ParamsShow a => ParamsShow (Maybe a) where render = maybe mempty render

instance {-# OVERLAPS #-} (ParamsShow a, ParamsShow b) => ParamsShow (a, b) where
  render (x, y) = render x <> ", " <> render y

instance {-# OVERLAPS #-} (ParamsShow a, ParamsShow b, ParamsShow c) => ParamsShow (a, b, c) where
  render x = render (x ^. _1) <> ", " <> render (x ^. _2) <> ", " <> render (x ^. _3)

instance {-# OVERLAPS #-} (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d) => ParamsShow (a, b, c, d) where
  render x = render (x ^. _1) <> ", " <> render (x ^. _2) <> ", " <> render (x ^. _3) <> ", " <> render (x ^. _4)

instance {-# OVERLAPS #-} (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d, ParamsShow e) => ParamsShow (a, b, c, d, e) where
  render x = render (x ^. _1) <> ", " <> render (x ^. _2) <> ", " <> render (x ^. _3) <> ", " <> render (x ^. _4) <> ", " <> render (x ^. _5)

instance {-# OVERLAPS #-} (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d, ParamsShow e, ParamsShow f) => ParamsShow (a, b, c, d, e, f) where
  render x = render (x ^. _1) <> ", " <> render (x ^. _2) <> ", " <> render (x ^. _3) <> ", " <> render (x ^. _4) <> ", " <> render (x ^. _5) <> ", " <> render (x ^. _6)

instance {-# OVERLAPS #-} (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d, ParamsShow e, ParamsShow f, ParamsShow g) => ParamsShow (a, b, c, d, e, f, g) where
  render x = render (x ^. _1) <> ", " <> render (x ^. _2) <> ", " <> render (x ^. _3) <> ", " <> render (x ^. _4) <> ", " <> render (x ^. _5) <> ", " <> render (x ^. _6) <> ", " <> render (x ^. _7)

instance {-# OVERLAPS #-} (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d, ParamsShow e, ParamsShow f, ParamsShow g, ParamsShow j) => ParamsShow (a, b, c, d, e, f, g, j) where
  render x = 
    render (x ^. _1) <> ", " <> 
    render (x ^. _2) <> ", " <> 
    render (x ^. _3) <> ", " <> 
    render (x ^. _4) <> ", " <> 
    render (x ^. _5) <> ", " <> 
    render (x ^. _6) <> ", " <> 
    render (x ^. _7) <> ", " <> 
    render (x ^. _8)

instance {-# OVERLAPS #-} 
  (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d, ParamsShow e, ParamsShow f, 
   ParamsShow g, ParamsShow j, ParamsShow k)
  => ParamsShow (a, b, c, d, e, f, g, j, k) where
  render x = 
    render (x ^. _1) <> ", " <> 
    render (x ^. _2) <> ", " <> 
    render (x ^. _3) <> ", " <> 
    render (x ^. _4) <> ", " <> 
    render (x ^. _5) <> ", " <> 
    render (x ^. _6) <> ", " <> 
    render (x ^. _7) <> ", " <> 
    render (x ^. _8) <> ", " <> 
    render (x ^. _9)


instance {-# OVERLAPS #-} 
  (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d, ParamsShow e, ParamsShow f, 
   ParamsShow g, ParamsShow j, ParamsShow k, ParamsShow l) 
  => ParamsShow (a, b, c, d, e, f, g, j, k, l) where
  render x = 
    render (x ^. _1) <> ", " <> 
    render (x ^. _2) <> ", " <> 
    render (x ^. _3) <> ", " <> 
    render (x ^. _4) <> ", " <> 
    render (x ^. _5) <> ", " <> 
    render (x ^. _6) <> ", " <> 
    render (x ^. _7) <> ", " <> 
    render (x ^. _8) <> ", " <> 
    render (x ^. _9) <> ", " <> 
    render (x ^. _10)


instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x)

instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m,
   ParamsShow n)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m, n) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x) <> ", " <>
    render (sel12 x)

instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m,
   ParamsShow n,
   ParamsShow o)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m, n, o) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x) <> ", " <>
    render (sel12 x) <> ", " <>
    render (sel13 x)

instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m,
   ParamsShow n,
   ParamsShow o,
   ParamsShow p)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m, n, o, p) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x) <> ", " <>
    render (sel12 x) <> ", " <>
    render (sel13 x) <> ", " <>
    render (sel14 x)

instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m,
   ParamsShow n,
   ParamsShow o,
   ParamsShow p,
   ParamsShow q)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m, n, o, p, q) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x) <> ", " <>
    render (sel12 x) <> ", " <>
    render (sel13 x) <> ", " <>
    render (sel14 x) <> ", " <>
    render (sel15 x)

instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m,
   ParamsShow n,
   ParamsShow o,
   ParamsShow p,
   ParamsShow q,
   ParamsShow r)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m, n, o, p, q, r) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x) <> ", " <>
    render (sel12 x) <> ", " <>
    render (sel13 x) <> ", " <>
    render (sel14 x) <> ", " <>
    render (sel15 x) <> ", " <>
    render (sel16 x)

instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m,
   ParamsShow n,
   ParamsShow o,
   ParamsShow p,
   ParamsShow q,
   ParamsShow r,
   ParamsShow s)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m, n, o, p, q, r, s) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x) <> ", " <>
    render (sel12 x) <> ", " <>
    render (sel13 x) <> ", " <>
    render (sel14 x) <> ", " <>
    render (sel15 x) <> ", " <>
    render (sel16 x) <> ", " <>
    render (sel17 x)

instance {-# OVERLAPS #-} 
  (ParamsShow a, 
   ParamsShow b, 
   ParamsShow c, 
   ParamsShow d, 
   ParamsShow e, 
   ParamsShow f, 
   ParamsShow g, 
   ParamsShow j, 
   ParamsShow k, 
   ParamsShow l, 
   ParamsShow m,
   ParamsShow n,
   ParamsShow o,
   ParamsShow p,
   ParamsShow q,
   ParamsShow r,
   ParamsShow s,
   ParamsShow t)
  => ParamsShow (a, b, c, d, e, f, g, j, k, l, m, n, o, p, q, r, s, t) where
  render x = 
    render (sel1 x) <> ", " <> 
    render (sel2 x) <> ", " <> 
    render (sel3 x) <> ", " <> 
    render (sel4 x) <> ", " <> 
    render (sel5 x) <> ", " <> 
    render (sel6 x) <> ", " <> 
    render (sel7 x) <> ", " <> 
    render (sel8 x) <> ", " <> 
    render (sel9 x) <> ", " <> 
    render (sel10 x) <> ", " <> 
    render (sel11 x) <> ", " <>
    render (sel12 x) <> ", " <>
    render (sel13 x) <> ", " <>
    render (sel14 x) <> ", " <>
    render (sel15 x) <> ", " <>
    render (sel16 x) <> ", " <>
    render (sel17 x) <> ", " <>
    render (sel18 x)

instance ParamsShow a => ParamsShow [a] where
  render xs = intercalate ", " $ map render xs

instance ParamsShow a => ParamsShow (V.Vector a) where
  render v = intercalate ", " $ map render (V.toList v)

-- instance ParamsShow a => ParamsShow (OnlyField symb a) where
--  render = render . coerce @_ @a
instance ParamsShow Value where
  render = show

instance ParamsShow UTCTime where render = show

instance ParamsShow Payload where
  render (Payload o) = show o

instance ParamsShow UUID where 
  render = show

statement :: ParamsShow a => Hasql.Statement a b -> a -> ReaderT KatipLoggerIO Session b
statement s@(Hasql.Statement sql _ _ _) a = do
  logger <- ask
  liftIO $ logger DebugS (ls (sql <> " { params: [" <> (render a ^. stext . textbs)) <> "] }")
  lift $ Hasql.statement a s

transactionIO :: Pool Hasql.Connection -> KatipLoggerIO -> ReaderT KatipLoggerIO Session a -> IO a
transactionIO pool logger session = runSessionIO pool logger session >>= either throwIO pure

transactionM :: forall m a . KatipContext m => Pool Hasql.Connection -> ReaderT KatipLoggerIO Session a -> m a
transactionM pool session = katipAddNamespace (Namespace ["db", "hasql"]) askLoggerIO >>= (liftIO . flip (transactionIO pool) session)