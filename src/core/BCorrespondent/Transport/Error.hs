{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module BCorrespondent.Transport.Error
  ( Error (..),
    AsError (..),
    Located (..),
    addMeta,
    appendMetas,
    appendMeta,
  )
where

import BCorrespondent.Transport.Payload
import Control.Exception
import Control.Lens hiding ((.=))
import Data.Aeson.Extended hiding (Error)
import Data.Bifunctor
import Data.Aeson.KeyMap
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
import Data.String (fromString)
import GHC.Generics
import Data.String.Conv (toS)
import Data.Default (Default, def)

-- | Generic error. Keeps information about the problem and
-- additional metadata.
data Error = Error
  { -- | Human readable message.
    errorMessage :: T.Text,
    -- | Message metadata.
    errorMeta :: Maybe Payload
  }
  deriving stock (Show)
  deriving stock (Generic)

instance Exception Error

instance Default Error where
  def = Error mempty Nothing

-- | How to convert concrete error into generic one.
class AsError e where
  asError :: e -> Error

instance AsError Error where asError = id

-- | Add metadata to the existing error.
addMeta :: ToJSON a => T.Text -> a -> Error -> Error
addMeta name t err = err { errorMeta = Just $ Payload $ singleton (fromString (toS name)) (toJSON t) }

-- | Append metadata to the list of arrays.
appendMeta :: ToJSON a => T.Text -> a -> Error -> Error
appendMeta name t Error {..} = error "append meta isn't implemented"

appendMetas :: ToJSON a => T.Text -> [a] -> Error -> Error
appendMetas name ts Error {..} = error "append metas isn't implemented"

instance ToJSON Error where
  toJSON Error {..} =
    object
      ( "message"
          .= errorMessage
          : case errorMeta of
            Nothing -> mempty
            Just x -> ["meta" .= x]
      )

instance FromJSON Error where
  parseJSON = withObject "error" $ \o -> do
    errorMessage <- o .: "message"
    errorMeta <- o .: "meta"
    pure Error {..}

instance AsError T.Text where
  asError t = Error t Nothing

instance ToSchema Error where
  declareNamedSchema _ = do
    text <- declareSchemaRef (Proxy @T.Text)
    obj <- declareSchemaRef (Proxy @Object)
    pure $
      NamedSchema (Just $ "Error") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ [("message", text), ("meta", obj)]
          & required .~ ["message"]

-- | Error with corresponding locations.
data Located at err = Located
  { locations :: [at],
    value :: err
  }
  deriving stock (Functor)

instance (ToJSON l, AsError v) => AsError (Located l v) where
  asError (Located l v) = appendMetas "source" l $ asError v

instance Bifunctor Located where
  first f (Located a b) = Located (f <$> a) b
  second g (Located a b) = Located a (g b)
