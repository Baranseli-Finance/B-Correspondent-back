{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hash (mkHash, mkHash512, mkHashWhirlpool, mkHashMd5) where

import "hashing" Crypto.Hash
import Data.Text
import Data.String.Conv (toS)

mkHashG :: forall al a . (HashAlgorithm  al, Show al, Show a) => a -> Text
mkHashG = toS . show . hash @al . toS . show

mkHash :: Show a => a -> Text
mkHash = mkHashG @SHA256

mkHash512 :: Show a => a -> Text
mkHash512 = mkHashG @SHA512

mkHashWhirlpool :: Show a => a -> Text
mkHashWhirlpool = mkHashG @Whirlpool

mkHashMd5 :: Show a => a -> Text
mkHashMd5 = mkHashG @MD5