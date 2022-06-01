{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Toon (getAgreementIds, getThermostatInfo, Agreement (..), AgreementId (..), Temperature (..)) where

import Data.Aeson (FromJSON (..))
import Data.ByteString.Lazy (ByteString)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest, requestHeaders, responseStatus, responseBody)
import Network.HTTP.Types.Status (statusIsSuccessful)
import GHC.Generics (Generic)
import qualified Data.Aeson as Json

import OAuth.Types (AccessToken (..))

data Agreement = Agreement
  { agreementId :: AgreementId
  -- lots of other crap
  } deriving (Show, Eq, Generic, FromJSON)

newtype AgreementId = AgreementId { unAgreementId :: String }
  deriving newtype (Show, Eq, FromJSON)

getAgreementIds :: AccessToken -> Manager -> IO [Agreement]
getAgreementIds tok mgr = do
  initialRequest <- parseRequest "https://api.toon.eu/toon/v3/agreements"
  let request = initialRequest
        { requestHeaders =
            [ ("Authorization", "Bearer " <> unAccessToken tok)
            , ("Content-Type", "application/json")
            , ("Cache-Control", "no-cache")
            ] ++ requestHeaders initialRequest
        }
  resp <- httpLbs request mgr
  if statusIsSuccessful (responseStatus resp)
  then decodeBody resp
  else  error (show resp)

data ThermostatInfo = ThermostatInfo
  { -- currentTemp: null?
    currentSetpoint :: Temperature
  , currentDisplayTemp :: Temperature
  , currentHumidity :: Int
  , programState :: ProgramState
  , activeState :: ActiveState
  , nextProgram :: ProgramState
  , nextState :: ActiveState
  , nextTime :: Time
  , nextSetpoint :: Temperature
  -- randomConfigId
  -- hasBoilerFault
  -- errorFound
  -- boilerModuleConnected
  , realSetpoint :: Temperature
  , burnerInfo :: BurnerState
  -- otCommError
  -- i2CError
  -- currentModulationLevel
  -- haveOTBoiler
  , lastUpdatedFromDisplay :: TimeMs
  -- setByLoadShifting
  } deriving (Show, Eq, Generic, FromJSON)

newtype Temperature = Temperature { unTemperature :: Rational }
  deriving Eq

instance Show Temperature where
  showsPrec p (Temperature t) = showsPrec p (realToFrac t :: Double)

instance FromJSON Temperature where
  parseJSON = fmap (Temperature . (/ 100)) . parseJSON

data ProgramState = ProgramOff | ProgramOn | Temporary
  deriving (Show, Eq, Enum)

instance FromJSON ProgramState where
  parseJSON = fmap toEnum . parseJSON

data ActiveState = Comfort | Home | Sleep | Away | Holiday
  deriving (Show, Eq, Enum)

instance FromJSON ActiveState where
  parseJSON = fmap toEnum . parseJSON

data BurnerState =  BurnerOff | BurnerOn | HotWater | Preheat
  deriving (Show, Eq, Enum)

instance FromJSON BurnerState where
  parseJSON = fmap (toEnum . read) . parseJSON

newtype Time = Time { unTime :: UTCTime }
  deriving (Show, Eq)

instance FromJSON Time where
  parseJSON = fmap (Time . posixSecondsToUTCTime . fromInteger) . parseJSON

newtype TimeMs = TimeMs { unTimeMs :: UTCTime }
  deriving (Show, Eq)

instance FromJSON TimeMs where
  parseJSON = fmap (TimeMs . posixSecondsToUTCTime . realToFrac . (/ (1000 :: Rational))) . parseJSON

getThermostatInfo :: AccessToken -> Manager -> AgreementId -> IO ThermostatInfo
getThermostatInfo tok mgr aid = do
  initialRequest <- parseRequest ("https://api.toon.eu/toon/v3/" <> unAgreementId aid <> "/thermostat")
  let request = initialRequest
        { requestHeaders =
            [ ("Authorization", "Bearer " <> unAccessToken tok)
            , ("Content-Type", "application/json")
            , ("Cache-Control", "no-cache")
            ] ++ requestHeaders initialRequest
        }
  resp <- httpLbs request mgr
  print (responseStatus resp)
  print (responseBody resp)
  if statusIsSuccessful (responseStatus resp)
  then decodeBody resp
  else  error (show resp)

type ResponseHandler a = Response ByteString -> IO a

decodeBody :: FromJSON a => ResponseHandler a
decodeBody resp =
  let valOrError = Json.eitherDecode (responseBody resp)
  in either error return valOrError -- TODO better error handling
