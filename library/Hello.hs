{-# LANGUAGE DeriveGeneric #-}
module Hello where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text.Lazy as L

data Hello = Hello
    { name :: String
    , message :: String
    } deriving(Show, Generic)

instance ToJSON Hello
instance FromJSON Hello


getHelloText :: Hello -> L.Text
getHelloText (Hello n m) =
    L.pack $ n ++ " " ++ m