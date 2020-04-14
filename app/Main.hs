{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import LoadEnv
import System.Environment
import qualified Data.ByteString as B
import Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getEnvBS :: String -> IO B.ByteString
getEnvBS = fmap (T.encodeUtf8 . T.pack) . getEnv

main :: IO ()
main = do
  loadEnv
  sid <- getEnvBS "TWILIO_ACCOUNT_SID"
  token <- getEnvBS "TWILIO_AUTH_TOKEN"
  manager <- newManager tlsManagerSettings
  let body = [("Body", "Hello World!"), ("From", "+12052728785"), ("To", "+994518010000")]
  let url = "https://api.twilio.com/2010-04-01/Accounts/" ++ BU.toString sid ++ "/Messages.json"
  initialRequest <- parseUrlThrow url
  let request = applyBasicAuth sid token $ urlEncodedBody body $ initialRequest {method = "POST"}
  response <- httpLbs request manager
  putStrLn $ "Response status: " ++ show (statusCode $ responseStatus response)
  --print $ responseBody response