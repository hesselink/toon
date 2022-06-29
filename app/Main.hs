{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Config (Config)
import OAuth
import Toon
import qualified CmdLine
import qualified Config

main :: IO ()
main = do
  cmdOpts <- CmdLine.getOpts
  cfg <- Config.get (CmdLine.configFile cmdOpts)
  getTemperature (Config.apiToken_ cfg)

-- doesn't seem to work, gives invalid code on the requestAccessToken
-- call.
oauthFlow :: Config -> IO ()
oauthFlow cfg = do
  authCode <- requestAuthorizationCode (Config.oauthClientId_ cfg)
  mgr <- newManager tlsManagerSettings
  tok <- requestAccessToken (Config.oauthClientId_ cfg) (Config.oauthClientSecret_ cfg) (Left authCode) mgr
  print tok

getTemperature :: AccessToken -> IO ()
getTemperature tok = do
  mgr <- newManager tlsManagerSettings
  [agreement] <- getAgreementIds tok mgr
  print agreement
  info <- getThermostatInfo tok mgr (agreementId agreement)
  print info
