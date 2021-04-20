-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
-- | An example module.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

-- | An example function.
import Network.Wai.Middleware.Cors
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Hello
import StringTools
import qualified Data.Text.Lazy as L

main :: IO ()
main = do -- IO Monad
  putStrLn "Startin Server at 4712 ..."
  scotty 4712 $ do -- ScottyM Monad
    middleware $ cors $ const $ Just appCorsResourcePolicy -- $ erstatter paranteser
    -- middleware (cors (const (just appCorsResourcePolicy)))
    get "/hello" $ do -- første endpoint (Localhost/hello)
      text "Hello World!" -- Return værdi af /hello endpoint
    post "/helloMessage" $ do
        hello <- jsonData :: ActionM Hello -- ActioM stammer fra scotty biblioteket. "Det gør det muligt at transformere jason data, ind til en haskell data type"?
        json $ getHelloText hello -- Retur værdi fra
    get "/countWords" $ do
        receivedData <- jsonData :: ActionM ReceivedData
        json $ makeData receivedData

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }

