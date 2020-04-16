module Network.Simple.Client
    ( Client (..)
    , buildApi
    ) where

import           Data.Aeson       (FromJSON)
import           Network.HTTP.Req (Option, Scheme, Url, req)
import qualified Network.HTTP.Req as Req

class Client a where
  type ClientScheme a :: Scheme
  baseUrl :: a -> Url (ClientScheme a)
  mkHeader :: a -> Option (ClientScheme a)

buildApi ::
  ( Req.MonadHttp m, Req.HttpMethod method, Req.HttpBody body
  , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body)
  , Client c, FromJSON response
  )
  => c                             -- ^ Client
  -> method                        -- ^ HTTP method
  -> Url (ClientScheme c)          -- ^ Location of resource
  -> body                          -- ^ Body of the request
  -> Option (ClientScheme c)       -- ^ Request params
  -> m (Req.JsonResponse response)
buildApi c method path body params =
  req method path body Req.jsonResponse (mkHeader c <> params)
