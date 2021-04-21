{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Hello
import Color
import StringTools
import ApplicativeTest
import qualified Data.Text.Lazy as L

main :: IO ()
main = do -- IO Monad
  putStrLn "Testing our cool functions!"
  putStrLn "Testing semigroups:"
  print $ mix Red Blue
  putStrLn "testing applicative"
  print idiotMaker
  print idiotMakerEvolved
  putStrLn "Startin Server at 4712 ..."
  scotty 4712 $ do -- ScottyM Monad
    middleware $ cors $ const $ Just appCorsResourcePolicy -- $ erstatter paranteser
    -- middleware (cors (const (just appCorsResourcePolicy)))
    get "/hello" $ do -- første endpoint (Localhost/hello)
      text "Hello World!" -- Return værdi af /hello endpoint
    post "/helloMessage" $ do
        hello <- jsonData :: ActionM Hello -- ActioM stammer fra scotty biblioteket. "Det gør det muligt at transformere JSON data, ind til en haskell data type"?
        json $ getHelloText hello -- Returner hello, kørt gennem getHelloText, som JSON 
    get "/countWords" $ do
        receivedData <- jsonData :: ActionM ReceivedData
        json $ makeData receivedData
    get "/employees" $ do
        json $ e1

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }

data Employee = Employee
  { id :: Int
  , firstName :: String
  , lastName :: String
  , email :: String
  , departmentcode :: Int
  } deriving (Show, Generic)

instance ToJSON Employee

e1 :: [Employee]
e1 = [Employee 1 "Børge" "Børgesen" "uwu@glomp.com" 69, Employee 2 "Børge" "Børgesen" "uwu@glomp.com" 69]
