{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Company (Company (Company), name, catchPhrase) where

import Database.SQLite.Simple.FromRow
import Data.Aeson

type Name = String
data Company = Company { name :: Name, catchPhrase :: String } deriving (Show)

instance FromRow Company where
  fromRow = Company <$> field <*> field

instance FromJSON Company where
  parseJSON = withObject "Company" $ \o -> do
    name <- o .: "name"
    catchPhrase <- o .: "catchPhrase"
    return Company{..}

instance ToJSON Company where
  toJSON Company{..} = object [
    "name" .= name,
    "catchPhrase" .= catchPhrase ]
