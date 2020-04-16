{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Extensible.FormUrlEncoded () where

import           Data.Extensible
import           Data.Functor.Identity (Identity)
import           Data.Int              (Int64)
import           Data.Text             (Text)
import           Web.FormUrlEncoded
import           Web.HttpApiData

instance Forall (KeyTargetAre KnownSymbol FromFormData) xs => FromForm (Record xs) where
  fromForm form =
    hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol FromFormData)) $ \m ->
      Field <$> parseUnique' (stringKeyOf m) form

class FromFormData a where
  parseUnique' :: Text -> Form -> Either Text a

instance FromFormData Int where parseUnique' = parseUnique
instance FromFormData Float where parseUnique' = parseUnique
instance FromFormData Bool where parseUnique' = parseUnique
instance FromFormData Char where parseUnique' = parseUnique
instance FromFormData Int64 where parseUnique' = parseUnique
instance FromFormData Text where parseUnique' = parseUnique

instance FromHttpApiData a => FromFormData (Maybe a) where
  parseUnique' key form = do
    mv <- lookupMaybe key form
    case mv of
      Just v  -> Just <$> parseQueryParam v
      Nothing -> pure Nothing

instance FromFormData a => FromFormData (Identity a) where
  parseUnique' key form = pure <$> parseUnique' key form
