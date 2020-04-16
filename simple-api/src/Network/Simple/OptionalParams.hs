module Network.Simple.OptionalParams
  ( OptionalParams
  , buildRequestParams
  , ToHttpApiData' (..)
  ) where

import           Data.Extensible
import           Data.Functor.Identity    (Identity (..))
import           Data.Maybe               (mapMaybe)
import           Data.Text                (Text, intercalate)
import           Network.HTTP.Req
import           Web.Internal.HttpApiData (ToHttpApiData (..))

class ToHttpApiData' a where
  toQueryParam' :: a -> Maybe Text

instance ToHttpApiData' Int   where toQueryParam' = Just . toQueryParam
instance ToHttpApiData' Float where toQueryParam' = Just . toQueryParam
instance ToHttpApiData' Bool  where toQueryParam' = Just . toQueryParam
instance ToHttpApiData' Text  where toQueryParam' = Just . toQueryParam

instance ToHttpApiData' a => ToHttpApiData' [a] where
  toQueryParam' [] = Nothing
  toQueryParam' xs = Just $ intercalate "," $ mapMaybe toQueryParam' xs

type OptionalParams xs = xs :& Nullable (Field Identity)

buildRequestParams ::
  ( Forall (KeyTargetAre KnownSymbol ToHttpApiData') xs
  , QueryParam param
  , Monoid param
  ) => OptionalParams xs -> param
buildRequestParams =
  hfoldMapWithIndexWith @ (KeyTargetAre KnownSymbol ToHttpApiData') $
    \m x -> maybe mempty (stringKeyOf m =:) $ toQueryParam' =<< unwrap x
