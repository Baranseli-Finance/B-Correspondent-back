{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module BCorrespondent.Statement.Institution.Auth (Institution (..), fetchToken, insertToken) where

import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Text (Text)
import Database.Transaction (ParamsShow (..))
import TH.Mk (mkArbitrary)
import GHC.Generics (Generic)
import Data.String.Conv (toS)
import Control.Lens (lmap, dimap)
import Data.Bifunctor (first)
import Data.Vector (fromList, toList)


data Institution = Elekse | Tochka
  deriving (Generic, Show)

instance ParamsShow Institution where
  render = show

mkArbitrary ''Institution


insertToken :: HS.Statement (Institution, Text) ()
insertToken = 
  lmap (first (toS . show)) 
  [resultlessStatement|
    insert into institution.auth_token 
    (institution, token) 
    values ($1 :: text, $2 :: text) 
    on conflict (institution) 
    do update set token = $2 :: text|]

fetchToken :: HS.Statement [Institution] [(Text, Text)]
fetchToken = dimap (fromList . map (toS . show)) toList [vectorStatement|select (lower(institution) || '_token') :: text, token :: text from institution.auth_token where institution = any($1 :: text[])|]