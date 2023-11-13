{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Crypto.Cipher.Symmetric
  ( genSecretKey,
    genRandomIV,
    encrypt,
    decrypt,
    twofish128,
    printKey,
    readKey,
    writeKey,
    writeRandomBytes,
    twofish128Key,
    twofish128IV,
    Key (..)
  )
where

import Crypto.Cipher.Twofish (Twofish128)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import Crypto.Error (CryptoError, eitherCryptoError)
import qualified Crypto.Random.Types as CRT
import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.String.Conv (toS)
import qualified Data.ByteString.Base64 as B64 
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- | Not required, but most general implementation
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

printKey :: Key c ByteString -> String
printKey (Key k) = toS . decodeUtf8 . B64.encode $ k

readKey :: forall c . BlockCipher c => FilePath -> IO (Either String (Key c ByteString))
readKey = fmap (fmap Key . B64.decode . encodeUtf8 . toS) . readFile

writeKey :: forall c . BlockCipher c => FilePath -> IO ()
writeKey path = fmap printKey (genSecretKey @c) >>= writeFile path

writeRandomBytes :: forall c . BlockCipher c => FilePath -> IO ()
writeRandomBytes path = do 
  d <- fmap (toS . decodeUtf8 . B64.encode) $ CRT.getRandomBytes $ blockSize (undefined :: c)
  writeFile path d

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genSecretKey :: forall c a m . (CRT.MonadRandom m, BlockCipher c, ByteArray a) => m (Key c a)
genSecretKey = fmap Key $ CRT.getRandomBytes $ blockSize (undefined :: c)

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall c m . (CRT.MonadRandom m, BlockCipher c) => m (Maybe (IV c))
genRandomIV = fmap (makeIV @ByteString) $ CRT.getRandomBytes $ blockSize (undefined :: c)

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = eitherCryptoError $ cipherInit k

encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt secretKey initIV msg = initCipher secretKey <&> \c -> ctrCombine c initIV msg

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decrypt = encrypt
{-# inline decrypt #-}

twofish128Key :: ByteString -> Key Twofish128 ByteString
twofish128Key = Key
{-# inline twofish128Key #-}

twofish128IV :: ByteString -> Maybe (IV Twofish128)
twofish128IV = makeIV
{-# inline twofish128IV #-}

twofish128 :: forall a . ByteArray a => Key Twofish128 a -> IV Twofish128 -> a -> Either CryptoError a
twofish128 = encrypt @Twofish128 @a
{-# inline twofish128 #-}


