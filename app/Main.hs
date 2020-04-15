{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

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

apiResponse :: Int -> String
apiResponse code
  | code >= 200 = "Message Sent Successfully"
  | otherwise = "Failed to send SMS"

main :: IO ()
main = do
  loadEnv
  sid <- getEnvBS "TWILIO_ACCOUNT_SID"
  token <- getEnvBS "TWILIO_AUTH_TOKEN"
  myNumber <- getEnvBS "MY_NUMBER"
  herNumber <- getEnvBS "HER_NUMBER"
  manager <- newManager tlsManagerSettings
  msgBody <-
    pick
      [ "Late at work. Working hard"
      , "Late at work. Gotta ship this feature"
      , "Late at work. Someone fucked up the system again"
      ]
  let body = [("Body", msgBody), ("From", myNumber), ("To", herNumber)]
  let url = "https://api.twilio.com/2010-04-01/Accounts/" ++ BU.toString sid ++ "/Messages.json"
  initialRequest <- parseRequest url
  let request = applyBasicAuth sid token $ urlEncodedBody body $ initialRequest {method = "POST"}
  response <- httpLbs request manager
  print (apiResponse (statusCode $ responseStatus response))