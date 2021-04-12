{-# LANGUAGE DeriveGeneric #-}
module StringTools where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text.Lazy as L
import Data.List (length, split)
import Data.Char


data ReceivedData = ReceivedData
    { text :: String
    , word :: String
    } deriving(Show, Generic)

data SendData = SendData
    { wordCount :: Int 
    , sendWord :: String 
    } deriving(Show, Generic)


-- instance ToJSON StringData
instance FromJSON ReceivedData
instance ToJSON SendData

countWords :: ReceivedData -> Int 
countWords (ReceivedData text word) = 
    let filteredList = filter (== word) $ splitOn " " $ cleanText text
    length filteredList 

cleanText :: String -> String
cleanText text = 
   map toLower $ filter (not . (`elem` ",.?!-:;\"\'")) text 
