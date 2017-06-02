{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module User (User, uid, name) where

import Data.Aeson
import qualified Company
import Company (Company)

data User = User {company :: Company, name :: String, uid :: Int} deriving (Show)

instance FromJSON User where
  parseJSON = withObject "Users" $ \o -> do
    name <- o .: "name"
    company <- o .: "company"
    uid  <- o .: "id"
    return User{..}

instance ToJSON User where
  toJSON User{..} = object [
    "name" .= name,
    "company" .= company,
    "id"  .= uid  ]
