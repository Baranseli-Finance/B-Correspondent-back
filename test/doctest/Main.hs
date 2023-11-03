module Main (main) where

-- import Build_doctest (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Test.DocTest (doctest)
import File (getFilesRecursive)
import System.FilePath.Posix (takeExtension)

main :: IO ()
main = do
  modulesCore <- fmap (filter ((== ".hs") . takeExtension)) $ getFilesRecursive "./src/core"
  modulesExt <- fmap (filter ((== ".hs") . takeExtension)) $ getFilesRecursive "./src/extended"
  print $ modulesExt <> modulesCore
  let args = "-isrc" : modulesCore <> modulesExt
  traverse_ putStrLn args -- optionally print arguments
  doctest args
