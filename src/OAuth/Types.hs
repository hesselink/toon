{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OAuth.Types
( AuthorizationCode (..)
, AccessTokenResponse (..)
, AccessToken (..)
, RefreshToken (..)
, ClientId (..)
, ClientSecret (..)
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import qualified Data.Aeson as Json

-- | These are requested through the user browser flow and obtained
-- from the redirect url.

newtype AuthorizationCode = AuthorizationCode { unAuthorizationCode :: ByteString }
  deriving (Show, Eq, Read)

-- | The response from requesting an access token.

data AccessTokenResponse = AccessTokenResponse
  { expiresIn :: DiffTime
  , accessToken :: AccessToken
  , refreshToken :: RefreshToken -- Only provided if requesting the "offline_access" scope.
  , scope :: String
  } deriving (Show, Eq)

instance FromJSON AccessTokenResponse where
  parseJSON = Json.withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
    <$> fmap secondsToDiffTime (v .: "expires_in")
    <*> v .: "access_token"
    <*> v .: "refresh_token"
    <*> v .: "scope"

instance ToJSON AccessTokenResponse where
  toJSON r = Json.object
    [ "expires_in" .= (floor . toRational $ expiresIn r :: Integer)
    , "access_token" .= accessToken r
    , "refreshToken" .= refreshToken r
    , "scope" .= scope r
    ]

-- | A short lived access token.

newtype AccessToken = AccessToken { unAccessToken :: ByteString }
  deriving (Show, Eq, Read)

instance ToJSON AccessToken where
  toJSON = toJSON . decodeUtf8 . unAccessToken

instance FromJSON AccessToken where
  parseJSON = fmap (AccessToken . encodeUtf8) . parseJSON

-- | A longer lived refresh token, can be used to request new access
-- tokens without going through the user flow.

newtype RefreshToken = RefreshToken { unRefreshToken :: ByteString }
  deriving (Show, Eq, Read)

instance ToJSON RefreshToken where
  toJSON = toJSON . decodeUtf8 . unRefreshToken

instance FromJSON RefreshToken where
  parseJSON = fmap (RefreshToken . encodeUtf8) . parseJSON

newtype ClientId = ClientId { unClientId :: String }
  deriving (Show, Eq, FromJSON)

newtype ClientSecret = ClientSecret { unClientSecret :: String }
  deriving (Show, Eq, FromJSON)
