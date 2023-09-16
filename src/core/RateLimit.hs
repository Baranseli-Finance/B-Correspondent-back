{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module RateLimit (KeyPolicy, module R) where

import Servant.RateLimit as R
import Network.Wai (Request, requestHeaders)
import qualified Data.ByteString as B
import Servant.Swagger
import Servant.API.Extended
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.String.Conv (toS)
import Data.Maybe (fromMaybe)
import Data.String (fromString)

data KeyPolicy (s :: Symbol)

instance KnownSymbol s => HasRateLimitPolicy (KeyPolicy s) where
  type RateLimitPolicyKey (KeyPolicy s) = B.ByteString
  policyGetIdentifier (req :: Request) = do 
    let token = fromString $ toS $ symbolVal (Proxy @s)
    pure $ fromMaybe mempty $ lookup token $ requestHeaders req

instance HasSwagger api => HasSwagger ((RateLimit strategy policy) :> api) where toSwagger _ = toSwagger (Proxy @api)