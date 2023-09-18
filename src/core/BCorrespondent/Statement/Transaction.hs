{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Transaction (insert) where

import qualified Hasql.Statement as HS

data Status = Registered | ForwardedToElekse | Confirmed | Declined

insert :: HS.Statement () ()
insert = undefined