{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Generic response that should be used in services. Currently it doesn't
-- keep type level info about concrete error that may happen, but that may
-- change in the future. Instead we use "Scaffold.Error" module to represent
-- generic errors without looking "inside".
--
-- Module provide 'Response' type and convenient pattern synonyms for
-- different pattern creation.
module BCorrespondent.Transport.Response
  ( Response (Response, Ok, Warnings, Errors, Error),
    BCorrespondent.AsError (..),
    fromValidation,
    fromEither,
    fromEithers,
    liftMaybe,
    toEither
  )
where

import BCorrespondent.Transport.Error as BCorrespondent
import Control.Lens hiding ((.=))
import Data.Aeson.Extended hiding (Error)
import Data.Swagger hiding (Response)
import qualified Data.Text as T
import Data.Typeable
import GHC.Exts
import GHC.Generics
import Test.QuickCheck.Extended
import Validation
import Data.Aeson.WithField (WithField (..))

--  Generic response for sirius services.
data Response a = Response
  { -- | Computation result.
    responseResult :: Maybe a,
    responseWarnings :: [Error],
    responseErrors :: [WithField "code" (Maybe Int) Error]
  }
  deriving stock (Show)
  deriving stock (Typeable)
  deriving stock (Generic)
  deriving stock (Foldable)
  deriving stock (Traversable)
  deriving stock (Functor)

pattern Ok :: a -> Response a
pattern Ok x = Response (Just x) ([] :: [BCorrespondent.Error]) ([] :: [WithField "code" (Maybe Int) BCorrespondent.Error])

pattern Warnings :: a -> [Error] -> Response a
pattern Warnings x warns = Response (Just x) warns ([] :: [WithField "code" (Maybe Int) BCorrespondent.Error])

pattern Errors :: [WithField "code" (Maybe Int) Error] -> Response a
pattern Errors errs = Response Nothing [] errs

pattern Error :: Maybe Int -> Error -> Response a
pattern Error code err = Response Nothing [] [WithField code err]

instance ToJSON a => ToJSON (Response a) where
  toJSON (Response Nothing [] []) = object ["success" .= Null]
  toJSON (Response Nothing ys xs) =
    object $
      (if null ys then id else (("warnings" .= ys) :))
        (["errors" .= xs | not (null xs)])
  toJSON (Response (Just x) ys xs) =
    object
      $ (if null ys then id else (("warnings" .= ys) :))
        . (if null xs then id else (("errors" .= xs) :))
      $ ["success" .= x]

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "response" $ \o -> do
    msuccess <- o .:? "success"
    warns <- o .:? "warnings" .!= []
    errors <- o .:? "errors" .!= []
    pure $ Response msuccess warns errors

instance (ToSchema a, Typeable a) => ToSchema (Response a) where
  declareNamedSchema _ = do
    e <- declareSchemaRef (Proxy @[WithField "code" (Maybe Int) Error])
    w <- declareSchemaRef (Proxy @[Error])
    v <- declareSchemaRef (Proxy @a)
    let ident = T.pack $ show (typeRep (Proxy @a))
    pure $
      NamedSchema (Just $ "Response_" <> ident) $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ fromList [ ("success", v), ("warnings", w), ("errors", e) ]

instance Arbitrary a => Arbitrary (Response a) where arbitrary = fmap (\x -> Response x [] []) arbitrary

fromValidation :: Validation [BCorrespondent.Error] a -> Response a
fromValidation v = either (Errors . map (WithField Nothing)) Ok $ validationToEither v

fromEither :: AsError e => Either e a -> Response a
fromEither = either (Errors . map (WithField Nothing) . flip (:) [] . asError) Ok

fromEithers :: AsError e => Either [e] a -> Response a
fromEithers = either (Errors . map (WithField Nothing) . map asError) Ok

liftMaybe :: AsError e => Maybe a -> e -> Response a
liftMaybe (Just x) _ = Ok x
liftMaybe Nothing e = BCorrespondent.Transport.Response.Error Nothing (asError e)

toEither :: Response a -> Either [Error] a
toEither (Response (Just x) _ _) = Right x
toEither (Response Nothing _ es) = Left $ flip map es $ \(WithField _ e) -> e