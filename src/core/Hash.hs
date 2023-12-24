{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hash (mkHash, mkHash512, mkHashWhirlpool, mkHashMd5) where

import "hashing" Crypto.Hash
import Data.Text
import Data.String.Conv (toS)

mkHashG :: forall al a . (HashAlgorithm  al, Show al, Show a) => (a -> String) -> a -> Text
mkHashG stringfy = toS . show . hash @al . toS . stringfy

mkHash :: Show a => (a -> String) -> a -> Text
mkHash stringfy = mkHashG @SHA256 stringfy

mkHash512 :: Show a => (a -> String) -> a -> Text
mkHash512 stringfy = mkHashG @SHA512 stringfy

mkHashWhirlpool :: Show a => (a -> String) -> a -> Text
mkHashWhirlpool stringfy = mkHashG @Whirlpool stringfy

mkHashMd5 :: Show a => (a -> String) -> a -> Text
mkHashMd5 stringfy = mkHashG @MD5 stringfy