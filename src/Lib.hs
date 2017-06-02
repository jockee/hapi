{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified User
import qualified Company
import User (User)
import Company (Company)
import Database.SQLite.Simple
import Network.HTTP.Simple
import qualified Control.Exception as CE
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

users :: L8.ByteString -> [User]
users xs = incUids dec
  where
    dec = decode xs :: Maybe [User]

loadJSON :: L8.ByteString -> String
loadJSON xs = L8.unpack . encode $ incUids dec
  where
    dec = decode xs :: Maybe [User]

incUids :: Maybe [User] -> [User]
incUids Nothing = []
incUids (Just users) = incOrDec <$> users
  where
    incOrDec user = incDec user $ head (User.name user) == 'C'
    incDec user True = user { User.uid = User.uid user - 9 }
    incDec user False = user { User.uid = User.uid user + 9 }

insertUsers :: Connection -> [User] -> IO ()
insertUsers conn = mapM_ insertUser
    where
      insertUser user = execute conn "INSERT INTO users (id, name, company_name, company_catchphrase) VALUES (?, ?, ?, ?)" (User.uid user, User.name user, cN user, cF user)
      cN user = Company.name $ User.company user
      cF user = Company.catchPhrase $ User.company user

someFunc :: IO ()
someFunc = do
  rsp <- httpLBS "http://jsonplaceholder.typicode.com/users"
  conn <- open "db/test.db"
  didInsert <- CE.try $ insertUsers conn . users $ getResponseBody rsp :: IO (Either SQLError ())
  case didInsert of
    Left ex -> putStrLn $ "Caught exception: " ++ show ex
    Right _ -> print "ok"
  r <- query_ conn "SELECT * from users" :: IO [User]
  mapM_ print r
  close conn
