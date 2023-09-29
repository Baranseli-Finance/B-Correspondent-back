{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Fs (insertFiles, InsertFile (..)) where

import qualified Hasql.Statement as HS
import Hasql.TH
import TH.Mk
import GHC.Generics
import Database.Transaction (ParamsShow (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Test.QuickCheck.Extended ()
import Data.Int (Int64)
import qualified Data.Vector as V
import Control.Lens
import Data.Tuple.Extended (app5)
import Data.Aeson (toJSON)

data InsertFile = 
     InsertFile 
     { insertFileHash :: T.Text, 
       insertFileName :: T.Text, 
       insertFileMime :: T.Text, 
       insertFileBucket :: T.Text, 
       insertFileExts :: [T.Text]
     }
     deriving (Generic, Show)

mkEncoder ''InsertFile
mkArbitrary ''InsertFile

encodeInsertFile = 
    fromMaybe (error "cannot encode InsertFile") 
  . mkEncoderInsertFile

instance ParamsShow InsertFile where
  render = render . encodeInsertFile

insertFiles :: HS.Statement [InsertFile] [Int64]
insertFiles = 
  dimap (
      V.unzip5 
    . V.map (
          app5 toJSON 
        . encodeInsertFile) 
    . V.fromList) 
    V.toList 
  [vectorStatement|
    insert into storage.file
    (hash, title, mime, bucket, exts)
    select x.hash, x.title, x.mime, x.bucket, x.exts
    from unnest(
      $1 :: text[], $2 :: text[], 
      $3 :: text[], $4 :: text[], 
      $5 :: json[]) 
      as x(hash, title, mime, bucket, exts)
    returning id :: int8|]
