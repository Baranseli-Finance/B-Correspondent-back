#!/usr/bin/env stack
-- stack script --resolver lts-21.4 --package cryptonite  --package bytestring --package base64-bytestring --package text
{-# LANGUAGE OverloadedStrings #-}

import Crypto.Random.Types (getRandomBytes)
import Crypto.PubKey.Ed448 (secretKeySize)
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import qualified Data.ByteString.Base64 as B64 
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO

main = do
 [file] <- getArgs
 ed448 <- fmap (decodeUtf8 . B64.encode) $ getRandomBytes secretKeySize
 Data.Text.IO.writeFile file ed448