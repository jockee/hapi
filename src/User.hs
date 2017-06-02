{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module User (User, uid, name, company) where

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Data.Aeson
import qualified Company
import Company (Company(Company))
import Control.Monad

type Name = String
data User = User { uid :: Int, name :: String, company :: Company} deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> liftM2 Company field field

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
