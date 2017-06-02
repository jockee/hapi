{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Company (Company, name, catchPhrase, bs) where

import Data.Aeson

data Company = Company { name :: String, catchPhrase :: String, bs :: String } deriving (Show)

instance FromJSON Company where
  parseJSON = withObject "Company" $ \o -> do
    name <- o .: "name"
    bs <- o .: "bs"
    catchPhrase <- o .: "catchPhrase"
    return Company{..}

instance ToJSON Company where
  toJSON Company{..} = object [
    "name" .= name,
    "catchPhrase" .= catchPhrase,
    "bs"  .= bs  ]
