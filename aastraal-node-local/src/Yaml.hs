{-# LANGUAGE OverloadedStrings #-}

module Yaml where

import Data.Yaml
-- import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as BS

data MyUser = MyUser {id :: Int,
                      name :: String,
                      reputation :: Int}
                      deriving (Show)

instance FromJSON MyUser where
    parseJSON (Object v) = MyUser <$>
                           v .: "id" <*>
                           v .: "name" <*>
                           v .: "reputation"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse MyUser from YAML/JSON"

main :: IO ()
main = do
         ymlData <- BS.readFile "users.yml"
         let users = Data.Yaml.decode ymlData :: Maybe [MyUser]
         -- Print it, just for show
         print $ fromJust users
