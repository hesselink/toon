{-# LANGUAGE DeriveGeneric #-}
module Config (Config (..), get) where

import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import GHC.Generics
import qualified Data.Yaml as Yaml

import OAuth.Types

data Config = Config
  { oauthClientId_ :: ClientId
  , oauthClientSecret_ :: ClientSecret
  , apiToken_ :: AccessToken
  , outputFile_ :: FilePath
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { oauthClientId_ = error "No default client ID."
  , oauthClientSecret_ = error "No default client secret."
  , apiToken_ = error "No default api token."
  , outputFile_ = "toon-data"
  }

data ParsedConfig = ParsedConfig
  { oauthClientId :: ClientId
  , oauthClientSecret :: ClientSecret
  , apiToken :: AccessToken
  , outputFile :: Maybe FilePath
  } deriving (Show, Generic)

instance FromJSON ParsedConfig

get :: FilePath -> IO Config
get cfgFile = either (error . show) (mergeWithDefault defaultConfig) <$> Yaml.decodeFileEither cfgFile

mergeWithDefault :: Config -> ParsedConfig -> Config
mergeWithDefault def pc = Config
 { oauthClientId_ = oauthClientId pc
 , oauthClientSecret_ = oauthClientSecret pc
 , apiToken_ = apiToken pc
 , outputFile_ = fromMaybe (outputFile_ def) (outputFile pc)
 }
