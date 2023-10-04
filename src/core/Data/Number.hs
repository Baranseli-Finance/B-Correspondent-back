{-# LANGUAGE CPP #-}

module Data.Number (lengthInt) where

#if MIN_VERSION_base(4,7,0)
import Data.Bits (FiniteBits(finiteBitSize))
#else
import Data.Bits (Bits(bitSize))
#endif

-- | Number of digits in a @number :: 'Int'@ in base 10.
lengthInt :: Int -> Int
lengthInt n = l32 `either32or64` l64
  where
    -- Same code as lengthInt64:
    l64
      | n == minBound = 19 -- "negate minBound" is out of range of Int64
      | n < 0         = go (negate n)
      | otherwise     = go n
      where
        -- Maximum is 9223372036854775807 for positive and 9223372036854775808
        -- for negative integer.
        go m
          | m < 10                 = 1
          | m < 100                = 2
          | m < 1000               = 3
          | m < 10000              = 4
          | m >= 10000000000000000 = 16 + go (m `quot` 10000000000000000)
          | m >= 100000000         = 8  + go (m `quot` 100000000)
          | otherwise              = 4  + go (m `quot` 10000)
            -- m >= 10000

    -- Same code as lengthInt32:
    l32
      | n == minBound = 10  -- "negate minBound" is out of range of Int32.
      | n < 0         = go (negate n)
      | otherwise     = go n
      where
        -- Maximum is 2147483647 for positive and 2147483648 for negative integer.
        go m
          | m < 10         = 1
          | m < 100        = 2
          | m < 1000       = 3
          | m < 10000      = 4
          | m >= 100000000 = 8 + go (m `quot` 100000000)
          | otherwise      = 4 + go (m `quot` 10000)

-- | Returns one of its arguments, depending on bit size of 'Word' type on
-- current hardware.
either32or64
    :: a
    -- ^ Used in case when 'Word' is 32bit long.
    -> a
    -- ^ Used in case when 'Word' is 64bit long.
    -> a
either32or64 on32bit on64bit = case wordSize (0 :: Word) of
    32 -> on32bit
    64 -> on64bit
    bs -> error $ "Data.NumberLength.either32or64: " <> show bs
        <> ": System uses Word size not supported by this library."
  where
    wordSize =
#if MIN_VERSION_base(4,7,0)
        finiteBitSize
#else
        bitSize
#endif
{-# INLINE either32or64 #-}