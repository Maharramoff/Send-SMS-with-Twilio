{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import LoadEnv
import System.Environment
import System.Random (randomRIO)
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


getEnvBS :: String -> IO B.ByteString
getEnvBS = fmap (T.encodeUtf8 . T.pack) . getEnv

pick :: [a] -> IO a
pick xs = (xs !!) Control.Applicative.<$> randomRIO (0, Prelude.length xs - 1)

main :: IO ()
main = do
  loadEnv
  sid <- getEnvBS "TWILIO_ACCOUNT_SID"
  token <- getEnvBS "TWILIO_AUTH_TOKEN"
  x <- pick ["Late at work. Working hard", "Late at work. Gotta ship this feature", "Late at work. Someone fucked up the system again"]
  manager <- newManager tlsManagerSettings
  let yourNumber = "+12052728785"
  let herNumber = "+994518010000"
  let body = [("Body", x), ("From", yourNumber), ("To", herNumber)]
  let url = "https://api.twilio.com/2010-04-01/Accounts/" ++ BU.toString sid ++ "/Messages.json"
  initialRequest <- parseUrlThrow url
  let request = applyBasicAuth sid token $ urlEncodedBody body $ initialRequest {method = "POST"}
  response <- httpLbs request manager
  putStrLn $ "Response status: " ++ show (statusCode $ responseStatus response)
  --print $ responseBody response