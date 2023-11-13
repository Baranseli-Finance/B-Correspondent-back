{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module BCorrespondent.Statement.Backup (insert) where

import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Text (Text)
import Control.Lens (rmap)

insert :: HS.Statement Text Bool
insert =
  rmap (> 0)
  [rowsAffectedStatement|
     insert into backup 
     (md5_checksum) 
     values ($1 :: text) 
     on conflict (md5_checksum) do nothing|]