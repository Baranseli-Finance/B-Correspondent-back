{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BCorrespondent.Explain (spec_explain) where

import qualified BCorrespondent.Statement.File
import qualified BCorrespondent.Statement.User.Auth

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Generics.Product.Positions
import Database.Migration.Test
import GHC.Generics
import Hasql.Decoders
import Hasql.Session
import Hasql.Statement
import Test.Hspec hiding (shouldBe)
import Test.Hspec.DB.Hasql
import Test.Hspec.Expectations.Lifted
import Test.QuickCheck (Arbitrary (arbitrary), generate)

spec_explain :: Spec
spec_explain =
  describeHasql
    [migrate]
    Nothing
    "explain"
    $ for_ explainTests
    $ \(modl, tests) ->
      context modl $
        for_ tests $
          \(name, ST st) ->
            itHasql name $ do
              let st' =
                    st
                      & position @1 %~ ("explain " <>)
                      & position @3 .~ noResult
              input <- liftIO $ generate arbitrary
              statement input st' >>= (`shouldBe` ())

deriving instance Generic (Statement a b)

-- | Existential wrapper for the query
data ST = forall a b. Arbitrary a => ST (Statement a b)

(==>) a b = (a, b)

(=>>) a b = (a, ST b)

-- | List of all database queries.
explainTests :: [(String, [(String, ST)])]
explainTests =
  [ "BCorrespondent.Statement.File" ==>
      [ "save" =>> BCorrespondent.Statement.File.save,
        "getMetaForReport" =>> BCorrespondent.Statement.File.getMetaForReport,
        "getMeta" =>> BCorrespondent.Statement.File.getMeta,
        "delete" =>> BCorrespondent.Statement.File.delete,
        "getHashWithBucket" =>> BCorrespondent.Statement.File.getHashWithBucket,
        "patch" =>> BCorrespondent.Statement.File.patch
      ]
  , "BCorrespondent.Statement.User.Auth" ==>
     [  "insertUser" =>> BCorrespondent.Statement.User.Auth.insertUser,
        "insertJwt" =>> BCorrespondent.Statement.User.Auth.insertJwt,
        "getUserIdByEmail" =>> BCorrespondent.Statement.User.Auth.getUserIdByEmail,
        "logout" =>> BCorrespondent.Statement.User.Auth.logout,
        "insertToken" =>> BCorrespondent.Statement.User.Auth.insertToken,
        "insertConfirmationLink" =>> BCorrespondent.Statement.User.Auth.insertConfirmationLink,
        "confirmEmail" =>> BCorrespondent.Statement.User.Auth.confirmEmail,
        "resendLink" =>> BCorrespondent.Statement.User.Auth.resendLink,
        "insertNewPassword" =>> BCorrespondent.Statement.User.Auth.insertNewPassword,
        "checkToken" =>> BCorrespondent.Statement.User.Auth.checkToken
     ]
  ]