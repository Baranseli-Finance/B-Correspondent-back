{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Mail (insert, fetchMail, update, Mail (..), Agent (..)) where

import Data.Text (Text)
import qualified Data.Text as T
import TH.Mk (mkArbitrary, mkEncoder)
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.UUID (UUID)
import Control.Lens (lmap)
import Data.Tuple.Extended (app4, snocT)
import Data.String.Conv (toS)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Test.QuickCheck.Extended ()
import Database.Transaction (ParamsShow (..))


data Agent = SendGrid | Google 
  deriving (Show, Generic)

mkArbitrary ''Agent

data Mail = 
     Mail 
     { mailUserEmail :: Text,
       mailSubject :: Text,
       mailBody :: Text, 
       mailAgent :: Agent 
     }
     deriving Generic

mkEncoder ''Mail
mkArbitrary ''Mail

encodeMail :: Mail -> (Text, Text, Text, Agent)
encodeMail = fromMaybe undefined . mkEncoderMail

instance ParamsShow Mail where render = show . encodeMail

insert :: HS.Statement Mail UUID
insert = 
  lmap (app4 (toS . show) . encodeMail)
  [singletonStatement|
    insert into email_journal
    (user_email, subject, body, agent)
    values ($1 :: text, $2 :: text, $3 :: text, $4 :: text)
    returning id :: uuid|]

fetchMail :: HS.Statement UUID (Maybe (Text, Text, Text))
fetchMail = [maybeStatement|select user_email :: text, subject :: text, body :: text from email_journal where id = $1 :: uuid|]

update :: HS.Statement (UUID, Maybe Text) ()
update = lmap (snocT (toS (show Google))) [resultlessStatement|update email_journal set agent = $3 :: text, reject_reason = $2 :: text? where id = $1 :: uuid|]
