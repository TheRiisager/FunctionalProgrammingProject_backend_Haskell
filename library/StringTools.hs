{-# LANGUAGE DeriveGeneric #-}
module StringTools where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.List.Split
import Data.Char


data ReceivedData = ReceivedData
    { receivedText :: String
    , word :: String
    } deriving(Show, Generic)

data SendData = SendData
    { wordCount :: Int 
    , sendWord :: String 
    } deriving(Show, Generic)

instance FromJSON ReceivedData
instance ToJSON SendData

countWords :: ReceivedData -> Int 
countWords (ReceivedData text word) = 
    length $ filter (== cleanText word) $ splitOn " " $ cleanText text


cleanText :: String -> String
cleanText text = 
   map toLower $ filter (not . (`elem` ",.?!-:;\"\'")) text 

makeData :: ReceivedData -> SendData
makeData dataReceived = 
    SendData (countWords dataReceived) (word dataReceived)
