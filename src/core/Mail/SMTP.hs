{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Mail.SMTP (send) where


import BCorrespondent.EnvKeys (SMTP (..))
import Data.Text (Text) 
import Network.Mail.SMTP
import Network.Mail.Mime 
       (Part (..), 
        Encoding (QuotedPrintableText),
        Disposition (DefaultDisposition), 
        PartContent (..)
       )
import Data.String.Conv (toS)


send :: SMTP -> Text -> Text -> Text -> IO ()
send SMTP {..} to subject body = 
  sendMailWithLoginTLS "smtp.gmail.com" (toS sMTPLogin) (toS sMTPCred) $ 
    simpleMail (Address Nothing sMTPLogin) [Address Nothing to] mempty mempty subject [part]
  where part = Part "text/plain; charset=utf-8" QuotedPrintableText DefaultDisposition mempty $ PartContent $ toS body