{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hash (mkHash, mkHash512, mkHashWhirlpool, mkHashMd5) where

import "hashing" Crypto.Hash
import Data.Text
import Data.String.Conv (toS)

mkHashG :: forall al a . (HashAlgorithm  al, Show al) => (a -> String) -> a -> Text
mkHashG stringfy = toS . show . hash @al . toS . stringfy

mkHash :: (a -> String) -> a -> Text
mkHash stringfy = mkHashG @SHA256 stringfy
{-# inline mkHash #-}

mkHash512 :: (a -> String) -> a -> Text
mkHash512 stringfy = mkHashG @SHA512 stringfy
{-# inline mkHash512 #-}

mkHashWhirlpool :: (a -> String) -> a -> Text
mkHashWhirlpool stringfy = mkHashG @Whirlpool stringfy
{-# inline mkHashWhirlpool #-}

mkHashMd5 :: (a -> String) -> a -> Text
mkHashMd5 stringfy = mkHashG @MD5 stringfy
{-# inline mkHashMd5 #-}