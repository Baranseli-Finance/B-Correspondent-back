{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Crypto.Cipher.Symmetric
  ( Blowfish256,
    Blowfish448,
    genSecretKey,
    genRandomIV,
    encrypt,
    decrypt
  )
where

import Crypto.Cipher.Blowfish (Blowfish256, Blowfish448)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import Crypto.Error (CryptoError, eitherCryptoError)
import qualified Crypto.Random.Types as CRT
import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))


-- | Not required, but most general implementation
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

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