{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified User
import User (User)
import Network.HTTP.Simple
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

loadJSON :: L8.ByteString -> String
loadJSON xs = L8.unpack . encode $ incUids dec
  where
    dec = decode xs :: Maybe [User]

incUids :: Maybe [User] -> Maybe [User]
incUids Nothing = Nothing
incUids (Just users) = Just $ incOrDec <$> [users !! 3]
  where
    incOrDec user = incDec user $ head (User.name user) == 'C'
    incDec user True = user { User.uid = User.uid user - 9 }
    incDec user False = user { User.uid = User.uid user + 9 }

someFunc :: IO ()
someFunc = do
  rsp <- httpLBS "http://jsonplaceholder.typicode.com/users"
  putStrLn . loadJSON $ getResponseBody rsp
