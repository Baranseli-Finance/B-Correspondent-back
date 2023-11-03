module Main (main) where

-- import Build_doctest (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Test.DocTest (doctest)

main :: IO ()
main = do
  let args = []
  traverse_ putStrLn args -- optionally print arguments
  doctest args
