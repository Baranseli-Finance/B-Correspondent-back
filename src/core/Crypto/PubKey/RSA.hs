{-# LANGUAGE PackageImports #-}

module Crypto.PubKey.RSA (RSAKey (..), generateRSAKey) where

import qualified "cryptonite" Crypto.PubKey.RSA as RSA
import qualified OpenSSL.PEM as SSL
import qualified OpenSSL.RSA as SSL

-- https://hackage.haskell.org/package/cryptonite-0.30/docs/Crypto-PubKey-RSA.html
data RSAKey = RSAKey { key :: RSA.PrivateKey, pem :: String } deriving (Show, Read)

openSslKeyToCryptoniteKey :: SSL.RSAKeyPair -> Maybe RSA.PrivateKey
openSslKeyToCryptoniteKey key = do 
  let d = SSL.rsaD key
  let p = SSL.rsaP key
  let q = SSL.rsaQ key
  let mdP = SSL.rsaDMP1 key
  let mdQ = SSL.rsaDMQ1 key
  let mqinv = SSL.rsaIQMP key 
  let size = SSL.rsaSize key
  let n = SSL.rsaN key
  let e = SSL.rsaE key

  dP <- mdP
  dQ <- mdQ
  qinv <- mqinv

  let pub = RSA.PublicKey size n e
  --   Represent a RSA private key.
  -- Only the pub, d fields are mandatory to fill.
  -- p, q, dP, dQ, qinv are by-product during RSA generation, but are useful to record here to speed up massively the decrypt and sign operation.
  -- implementations can leave optional fields to 0.
  return $ RSA.PrivateKey pub d p q dP dQ qinv 

generateRSAKey :: Int -> IO (Maybe RSAKey)
generateRSAKey bits = do 
  keys <- SSL.generateRSAKey bits 65537 Nothing -- change '65537' and 'Nothing' to the parameters you'd like to have
  pem <- SSL.writePublicKey keys
  pure $ fmap (`RSAKey` pem) $ openSslKeyToCryptoniteKey keys