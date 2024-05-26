{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( someFunc
  ) where

import           API                      (Device (..), Service, getDevice,
                                           initService)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (FromJSON (..), ToJSON (..),
                                           Value (..), object, (.:), (.=))
import           Data.Aeson.Types         (typeMismatch)
import           Data.Default.Class       (def)
import           Data.String              (fromString)
import qualified Data.Yaml                as Y
import           Network.HTTP.Types       (status400)
import           Network.Wai.Handler.Warp (setHost, setPort)
import           Options.Applicative
import           Web.Scotty               (ActionM, ScottyM, formParam, json,
                                           post, scottyOpts, settings, status,
                                           text)

newtype MountPoint = MountPoint String deriving (Show)

instance ToJSON MountPoint where
  toJSON (MountPoint uuid) = object [ "mountpoint" .= uuid ]

data User = SuperAdmin
    | Admin MountPoint
    | Normal MountPoint
    deriving (Show)

instance ToJSON User where
  toJSON SuperAdmin               = object ["type" .= ("superadmin" :: String)]
  toJSON (Admin (MountPoint mp))  = object ["type" .= ("admin" :: String), "mountpoint" .= mp]
  toJSON (Normal (MountPoint mp)) = object ["type" .= ("normal" :: String), "mountpoint" .= mp]

data Config = Config
    { service       :: Service
    , admin         :: String
    , adminPassword :: String
    , srvPort       :: Int
    , srvHost       :: String
    }

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "service"
           <*> v .: "admin"
           <*> v .: "password"
           <*> v .: "port"
           <*> v .: "host"
  parseJSON invalid    = typeMismatch "Config" invalid

lookupUser :: Config -> Service -> String -> String -> IO (Maybe User)
lookupUser Config {..} gw key token =
  if admin == key then
    if adminPassword == token then pure $ Just SuperAdmin
                              else pure Nothing
  else do
    mdev <- getDevice gw $ "token_" ++ token
    case mdev of
      Nothing  -> pure Nothing
      Just dev ->
        pure $ Just $ Normal $ MountPoint $ "/" ++ key ++ "/" ++ devUUID dev

authReqHandler :: Config -> Service -> ActionM ()
authReqHandler config gw = do
  key <- formParam "username"
  token <- formParam "password"
  r <- liftIO $ lookupUser config gw key token
  case r of
    Nothing -> do
      status status400
      json $ object [ "err" .= ("no auth" :: String)]
    Just u -> json u

superReqHandler :: ActionM ()
superReqHandler = text "ok"

aclReqHandler :: ActionM ()
aclReqHandler = text "ok"

application :: Config -> Service -> ScottyM ()
application config gw = do
  post "/mqtt/acl" aclReqHandler
  post "/mqtt/superuser" superReqHandler
  post "/mqtt/auth" $ authReqHandler config gw

newtype Options = Options {getConfigFile :: String}

parser :: Parser Options
parser = Options <$> strOption (long "config"
                               <> short 'c'
                               <> metavar "FILE"
                               <> help "config file."
                               <> value "config.yaml")

someFunc :: IO ()
someFunc = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Emqx auth"
     <> header "emqx-auth - Emqx auth" )

program :: Options -> IO ()
program Options{getConfigFile=configPath} = do
  c <- Y.decodeFileEither configPath
  case c of
    Left e     -> print e
    Right conf@Config{..} -> do
      let opts = def {settings = setPort srvPort
                               $ setHost (fromString srvHost) (settings def)}

      gw <- initService service
      scottyOpts opts $ application conf gw
