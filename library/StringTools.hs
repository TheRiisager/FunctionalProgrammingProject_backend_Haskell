{-# LANGUAGE DeriveGeneric #-}
module StringTools where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.List.Split ( splitOn )
import Data.Char ( toLower )

data ReceivedData = ReceivedData
    { receivedText :: String
    , word :: String
    } deriving(Show, Generic)

instance FromJSON ReceivedData

data SendData = SendData
    { wordCount :: Int 
    , sendWord :: String 
    } deriving(Show, Generic)

instance ToJSON SendData

--Takes the cleaned text, and splits the string in on spaces, then filters out all the words that are not the designated word. Lastly it returns the length of that
countWords :: ReceivedData -> Int 
countWords (ReceivedData text word) = 
    length $ filter (== cleanText word) $ splitOn " " $ cleanText text

--Indeholder ond Haskell magi fra Stackoverflow
cleanText :: String -> String
cleanText text = 
   map toLower $ filter (not . (`elem` ",.?!-:;\"\'")) text

makeData :: ReceivedData -> SendData
makeData dataReceived = 
    SendData (countWords dataReceived) (word dataReceived)




