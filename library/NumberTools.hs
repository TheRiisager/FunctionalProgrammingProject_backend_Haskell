{-# LANGUAGE DeriveGeneric #-}
module StringTools where
import GHC.Generics

data ReceivedNumbers = ReceivedNumbers
    { firstNumber :: Int
    , secondNumber :: Int
    } deriving(Show, Generic)