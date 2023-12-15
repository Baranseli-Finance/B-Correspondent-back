module BCorrespondent.Institution.Query (module F, queries) where

import BCorrespondent.Institution.Query.Factory as F
import qualified BCorrespondent.Institution.Query.Elekse.Token as E
import qualified BCorrespondent.Institution.Query.Elekse.Request as E
import Data.Int (Int64)

queries :: [(Int64, Query)]
queries = [(2, elekse)]

elekse :: Query
elekse = Query { fetchToken = E.fetch, makeRequest = E.make }