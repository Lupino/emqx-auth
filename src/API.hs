{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API
  ( getDevice
  , Device (..)
  , Service
  , initService
  ) where


import           Control.Exception   (SomeException, try)
import           Data.Aeson          (FromJSON (..), eitherDecode', withObject,
                                      (.!=), (.:), (.:?))
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      managerConnCount, managerResponseTimeout,
                                      newManager, parseRequest, requestHeaders,
                                      responseBody, responseTimeoutMicro)
import qualified Network.HTTP.Client as HTTP (httpLbs)
import           Network.HTTP.Types  (RequestHeaders)
import           System.Log.Logger   (errorM)

data Service = Service
  { host      :: String
  , timeout   :: Int
  , connCount :: Int
  , mgr       :: Manager
  }

instance Show Service where
  show a = concat [ "host = ", host a ]

initService :: Service -> IO Service
initService gw@Service{..} = do
  mgr' <- newManager defaultManagerSettings
    { managerConnCount = connCount
    , managerResponseTimeout = responseTimeoutMicro $ timeout * 1000
    }
  pure gw { mgr = mgr' }


instance FromJSON Service where
  parseJSON = withObject "Service" $ \o -> do
    host       <- o .:? "host"       .!= "http://localhost:3000"
    timeout    <- o .:? "timeout"    .!= 30
    connCount  <- o .:? "conn-count" .!= 10
    return Service
      { mgr = error "uninitial"
      , ..
      }

headers :: RequestHeaders
headers =
    [ ("User-Agent", "EmqxAuth/1.0.0")
    , ("Content-Type", "application/json")
    ]

data Device = Device
  { devUUID :: String
  , devKey  :: String
  }
  deriving (Show)

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o -> do
    devKey   <- o .: "key"
    devUUID  <- o .: "uuid"
    return Device{..}


data DeviceRsp = DeviceRsp
  { device :: Maybe Device
  , errMsg :: Maybe String
  }
  deriving (Show)

instance FromJSON DeviceRsp where
  parseJSON = withObject "Device" $ \o -> do
    device <- o .:? "device"
    errMsg <- o .:? "err"
    return DeviceRsp{..}

-- get "/api/devices/:ident/"
getDevice :: Service -> String -> IO (Maybe Device)
getDevice gw ident = do
  req <- parseRequest uri
  ersp <- try $ HTTP.httpLbs req {requestHeaders = headers} manager
  case ersp of
    Left (e :: SomeException) -> do
      errorM "API.getDevice" $ "getDevice error: " ++ show e
      pure Nothing
    Right rsp ->
      case eitherDecode' (responseBody rsp) of
        Left e    -> do
          errorM "API.getDevice" $ "getDevice error: " ++ show e
          pure Nothing
        Right val ->
          case errMsg val of
            Nothing -> pure $ device val
            Just err -> do
              errorM "API.getDevice" $ "getDevice error: " ++ err
              pure Nothing

  where uri = concat [ host gw, "/api/devices/", ident, "/" ]
        manager = mgr gw
