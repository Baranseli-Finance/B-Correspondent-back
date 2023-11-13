#!/usr/bin/env stack
-- stack script --resolver lts-21.4 --package cryptonite --package bytestring --package base64-bytestring --package text --package string-conv
{-# LANGUAGE OverloadedStrings #-}

import Crypto.Cipher.Twofish (Twofish128)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import qualified Crypto.Random.Types as CRT
import System.Environment (getArgs)
import Data.String.Conv (toS)
import qualified Data.ByteString.Base64 as B64 
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

main = do
 [path] <- getArgs
 d <- fmap (toS . decodeUtf8 . B64.encode) $ CRT.getRandomBytes $ blockSize (undefined :: Twofish128)
 writeFile path d