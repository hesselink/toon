{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time (getCurrentTime)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as Json

import Config (Config)
import OAuth
import Toon
import qualified CmdLine
import qualified Config

main :: IO ()
main = do
  cmdOpts <- CmdLine.getOpts
  cfg <- Config.get (CmdLine.configFile cmdOpts)
  getTemperature (Config.apiToken_ cfg) (Config.outputFile_ cfg)

-- doesn't seem to work, gives invalid code on the requestAccessToken
-- call.
oauthFlow :: Config -> IO ()
oauthFlow cfg = do
  authCode <- requestAuthorizationCode (Config.oauthClientId_ cfg)
  mgr <- newManager tlsManagerSettings
  tok <- requestAccessToken (Config.oauthClientId_ cfg) (Config.oauthClientSecret_ cfg) (Left authCode) mgr
  print tok

getTemperature :: AccessToken -> FilePath -> IO ()
getTemperature tok outFile = do
  mgr <- newManager tlsManagerSettings
  [agreement] <- getAgreementIds tok mgr
  print agreement
  info <- getThermostatInfo tok mgr (agreementId agreement)
  print info
  appendToFile outFile info

appendToFile :: Json.ToJSON a => FilePath -> a -> IO ()
appendToFile fp dat = do
  t <- getCurrentTime
  BS.appendFile fp (BS.pack (show t) <> "\n")
  BS.appendFile fp (Json.encode dat <> "\n")
