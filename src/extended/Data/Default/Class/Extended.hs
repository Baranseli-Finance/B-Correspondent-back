{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Default.Class.Extended (module Data.Default.Class) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Vector.Extended as V
import Data.Time.Clock (UTCTime (..))
import Data.UUID (UUID)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)

instance Default T.Text where
  def = T.empty

instance Default LT.Text where
  def = LT.empty

instance Default B.ByteString where
  def = B.empty

instance Default BL.ByteString where
  def = BL.empty

instance Default a => Default (V.Vector a) where def = V.replicate 10 def

instance Default Bool where
  def = False

instance Default UTCTime where
  def = UTCTime (read "1970-01-01") 0

instance Default UUID where
  def = unsafePerformIO randomIO