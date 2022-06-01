{-# LANGUAGE DeriveGeneric #-}
module Config (Config (..), get) where

import Data.Aeson (FromJSON)
import GHC.Generics
import qualified Data.Yaml as Yaml

import OAuth.Types

data Config = Config
  { oauthClientId :: ClientId
  , oauthClientSecret :: ClientSecret
  , apiToken :: AccessToken
  } deriving (Show, Generic)

instance FromJSON Config

get :: FilePath -> IO Config
get cfgFile = either (error . show) id <$> Yaml.decodeFileEither cfgFile
