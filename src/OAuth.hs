{-# LANGUAGE OverloadedStrings #-}
module OAuth (requestAuthorizationCode, requestAccessToken, AccessTokenResponse (..), AccessToken (..)) where

import Network.HTTP.Client (Manager, parseUrlThrow, urlEncodedBody, httpLbs, responseBody)
import Network.URI (escapeURIString, isUnescapedInURIComponent)
import qualified Data.Aeson as Json
import qualified Data.ByteString.UTF8 as UTF8

import OAuth.Types

redirectUri :: String
redirectUri = "http://localhost:8080/authorize"

requestAuthorizationCode :: ClientId -> IO AuthorizationCode
requestAuthorizationCode (ClientId oauthClientId) = do
  putStrLn $ "Please visit https://api.toon.eu/authorize?response_type=code&redirect_uri=" ++ escapeURIString isUnescapedInURIComponent redirectUri ++ "&client_id=" ++ oauthClientId ++ "&issuer=identity.toon.eu&tenant_id=eneco"
  putStrLn $ "Paste in the code:"
  code <- getLine
  return $ AuthorizationCode (UTF8.fromString code)

requestAccessToken :: ClientId -> ClientSecret -> Either AuthorizationCode RefreshToken -> Manager -> IO AccessTokenResponse
requestAccessToken (ClientId oauthClientId) (ClientSecret oauthClientSecret) codeOrToken mgr = do
  -- TODO error handling
  request <- parseUrlThrow "POST https://api.toon.eu/token"
  let codeParams = case codeOrToken of
        Left (AuthorizationCode code) ->
          [ ("grant_type", "authorization_code")
          , ("code", code)
          ]
        Right (RefreshToken tok) ->
          [ ("grant_type", "refresh_token")
          , ("refresh_token", tok)
          ]
      requestParams =
        [ ("client_id", UTF8.fromString oauthClientId)
        , ("client_secret", UTF8.fromString oauthClientSecret)
        , ("redirect_uri", UTF8.fromString redirectUri)
        , ("issuer", "identity.toon.eu")
        , ("tenant_id", "eneco")
        ] ++ codeParams
      requestWithBody = urlEncodedBody requestParams request
  print requestWithBody
  print requestParams
  resp <- httpLbs requestWithBody mgr
  let tokenOrError = Json.eitherDecode (responseBody resp)
  either error return tokenOrError -- TODO

