{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module BuildInfo where

import Data.Bifunctor
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T
import Data.Yaml
import Language.Haskell.TH (Exp (..), Lit (..))
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Syntax
  ( lift,
    loc_module,
    qLocation,
  )
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import System.Info
import qualified Data.Version as V

gitCommit :: ExpQ
gitCommit = lift $ unsafePerformIO $ (head . lines) `fmap` readProcess "git" ["log", "-1", "--format=%h"] mempty

gitTag :: ExpQ
gitTag = lift $ unsafePerformIO $ fromMaybe "-" . listToMaybe . lines <$> readProcess "git" ["tag", "--list", "--sort=-creatordate"] mempty

location :: ExpQ
location = [|fromString $((LitE . StringL . loc_module) `fmap` qLocation)|]

newtype Version = Version T.Text

instance Show Version where
  show (Version v) = T.unpack v

newtype Name = Name T.Text

instance Show Name where
  show (Name n) = T.unpack n

data Package = Package { packageVersion :: !Version, packageName :: !Name }

instance FromJSON Package where
  parseJSON = 
    withObject "Version" $ \o -> do
      packageVersion <- fmap Version $ o .: "version"
      packageName <- fmap Name $ o .: "name"
      pure Package {..}

getPackage :: IO (Either String Package)
getPackage = first prettyPrintParseException <$> decodeFileEither @Package "package.yaml"

getSystemInfo :: String
getSystemInfo = "os: " <> os <> "-" <> arch <> ", compiler: " <> compilerName <> "-" <> V.showVersion fullCompilerVersion