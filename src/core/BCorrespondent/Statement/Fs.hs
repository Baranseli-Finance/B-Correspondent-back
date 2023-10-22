{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Fs (insertFiles, fetchFiles, File (..)) where

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

data File = 
     File 
     { fileHash :: T.Text, 
       fileName :: T.Text, 
       fileMime :: T.Text, 
       fileBucket :: T.Text, 
       fileExts :: [T.Text]
     }
     deriving (Generic, Show)

mkEncoder ''File
mkArbitrary ''File

encodeFile = 
    fromMaybe (error "cannot encode File") 
  . mkEncoderFile

instance ParamsShow File where
  render = render . encodeFile

insertFiles :: HS.Statement [File] [Int64]
insertFiles = 
  dimap (
      V.unzip5 
    . V.map (
          app5 toJSON 
        . encodeFile) 
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

fetchFiles :: HS.Statement [Int64] [File]
fetchFiles = undefined
