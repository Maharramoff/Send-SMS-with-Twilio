{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager, tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let body = [("Body", "Hello World!"), ("From", "+12052728785"), ("To", "+994518010000")]
  let url  = "https://api.twilio.com/2010-04-01/Accounts/ACa38ae9fef48d112839874f279fddcb94/Messages.json"
  initialRequest <- parseUrlThrow url
  let request = applyBasicAuth "ACa38ae9fef48d112839874f279fddcb94" "6565eb69a6a8922d4689caae6436bb4b" $
                urlEncodedBody body $
                initialRequest {method = "POST"}
  response <- httpLbs request manager
  putStrLn $ "Response status: " ++ show (statusCode $ responseStatus response)
  --print $ responseBody response