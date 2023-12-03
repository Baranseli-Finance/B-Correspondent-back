{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Migration.Test (migrate) where

import Data.ByteString
import Data.Foldable
import Data.String.Interpolate
import Hasql.Session
import TH.Mk

$(mkMigrationTest ["23", "25"])

migrate :: Session ()
migrate = sql $ exts <> fold list
  where
    exts =
      [i|create extension postgres_fdw;
         create extension hstore;
         create extension ltree;
         create extension pg_trgm;
         create extension pgcrypto;
         create extension "uuid-ossp";|]
