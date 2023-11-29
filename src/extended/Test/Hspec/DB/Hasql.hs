{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Hspec.DB.Hasql (TestDBHasql, itHasql, describeHasql, session) where

import Control.Applicative ((<|>))
import Control.Exception (bracketOnError, finally)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import Data.List
import qualified Database.Postgres.Temp as Temp
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import Test.Hspec as HSpec

data TestDBHasql = TestDBHasql
  { -- | handle for temporary @postgres@ process
    tempDB :: !Temp.DB,
    -- | connections to the temporary @postgres@
    conn :: !Hasql.Connection
  }

throwError e = error $ "Error during db initialization: " <> show e

-- | Start a temporary postgres process and create a connections to it.
setupDBHasql ::
  [(String, String)] ->
  [Hasql.Session ()] ->
  -- | Data population.
  Maybe (Hasql.Session ()) ->
  IO TestDBHasql
setupDBHasql cfgXs migrations mpopulate = do
  let cfg = Temp.autoExplainConfig 0 <> mempty { Temp.postgresConfigFile = cfgXs }
  putStrLn $ "temp db config: " <> Temp.prettyPrintConfig cfg
  bracketOnError
    ( Temp.startConfig cfg >>= \case
        Left e -> throwError e
        Right db ->
          Temp.restart db >>= \case
            Left e -> throwError e
            Right db -> pure db
    )
    Temp.stop
    $ \tempDB -> do
      let connStr = Temp.toConnectionString tempDB
      Hasql.acquire connStr >>= \case
        Left e -> error $ "Error connecting to db" <> show e
        Right conn ->
          do
            es <- fmap lefts $ traverse (`Hasql.run` conn) migrations
            let migr =
                  case es of
                    [] -> Right ()
                    es -> Left $ intercalate "," $ map show es
            pop <- traverse (`Hasql.run` conn) mpopulate
            case Just migr ^? _Just . _Left <|> pop ^? _Just . _Left . to show of
              Just e -> liftIO (print $ "error while setup db: " <> e) >> error mempty
              Nothing -> pure TestDBHasql {..}

-- | Tear down DB Hasql.
tearDownDBHasql :: TestDBHasql -> IO ()
tearDownDBHasql TestDBHasql {..} = Hasql.release conn `finally` void (Temp.stop tempDB)

-- | Run session helper, throws a error, in session
-- errors out.
runSession :: Hasql.Session a -> TestDBHasql -> IO a
runSession action db = (action `Hasql.run` conn db) >>= either (error . show) return

-- | Wrapper for hasql test.
itHasql :: String -> Hasql.Session a -> SpecWith TestDBHasql
itHasql str action = str `HSpec.it` (void . runSession action)

-- | Run a hasql session bases test. Test takes a function that
-- can run the `Session` in on the current connection.
session :: String -> ((forall a. Hasql.Session a -> IO (Either Hasql.QueryError a)) -> IO a) -> SpecWith TestDBHasql
session name f = HSpec.it name (void . test)
  where
    test db = f (`Hasql.run` conn db)

-- | Hasql test.
describeHasql ::
  -- | add config vars
  [(String, String)] ->
  -- | Database initialization.
  [Hasql.Session ()] ->
  -- | Database population.
  Maybe (Hasql.Session ()) ->
  -- | Test name.
  String ->
  -- | Test itself.
  SpecWith TestDBHasql ->
  Spec
describeHasql cfgXs migrations mpopulate str =
  beforeAll (setupDBHasql cfgXs migrations mpopulate) . afterAll tearDownDBHasql . HSpec.describe str
