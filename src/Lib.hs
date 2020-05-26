{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( someFunc
  ) where

import           Control.Concurrent.STM   (TVar, atomically, newTVarIO,
                                           readTVarIO, writeTVar)
import           Control.Exception        (try)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (FromJSON (..), ToJSON (..),
                                           Value (..), object, (.:), (.=))
import           Data.Aeson.Types         (typeMismatch)
import           Data.Default.Class       (def)
import           Data.List                (find)
import           Data.String              (fromString)
import qualified Data.Yaml                as Y
import           Network.HTTP.Types       (status400)
import           Network.Wai.Handler.Warp (setHost, setPort)
import           Network.Wreq             (getWith)
import           Options.Applicative
import qualified System.Log.Logger        as Log
import           Web.Scotty               (ActionM, ScottyM, json, param, post,
                                           scottyOpts, settings, status, text)
import           Yuntan.Base              (Gateway (..), getOptionsAndSign,
                                           initGateway)
import           Yuntan.Types.Result      (ErrResult (errMsg), OkResult (..))
import           Yuntan.Utils.Wreq        (responseOkResult_)

newtype UUID = UUID String deriving (Show)

instance FromJSON UUID where
  parseJSON (Object v) = UUID <$> v .: "uuid"
  parseJSON invalid    = typeMismatch "UUID" invalid

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

data Service = Service
    { endpoint :: Gateway
    , password :: String
    }
    deriving (Show)

instance FromJSON Service where
  parseJSON (Object v) = Service <$> v .: "endpoint" <*> v .: "password"
  parseJSON invalid    = typeMismatch "Service" invalid


type ServiceList = TVar [Service]


data Config = Config
    { servicePath   :: FilePath
    , admin         :: String
    , adminPassword :: String
    , srvPort       :: Int
    , srvHost       :: String
    }

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "services"
           <*> v .: "admin"
           <*> v .: "password"
           <*> v .: "port"
           <*> v .: "host"
  parseJSON invalid    = typeMismatch "Config" invalid

initService :: Service -> IO Service
initService service = do
  newEndpoint <- initGateway $ endpoint service
  return service { endpoint = newEndpoint }

-- initConfig :: Config -> IO Config
-- initConfig config = do
--   newServices <- mapM initService $ services config
--   return config { services = newServices }

getService :: [Service] -> String -> Maybe Service
getService srvs key = find findFunc srvs
  where findFunc v = appKey (endpoint v) == key


loadService :: FilePath -> ServiceList -> IO ()
loadService fn ref = do
  s <- Y.decodeFileEither fn
  case s of
    Left e     -> print e
    Right srvs -> do
      srvs' <- mapM initService srvs
      atomically $ writeTVar ref srvs'


-- get   "/api/devices/:uuidOrToken/"
getUUID :: Gateway -> String -> IO (OkResult UUID)
getUUID gw token = do
  opts <- getOptionsAndSign "GET" path [] gw
  responseOkResult_ "device" $ getWith opts uri
  where path = concat [ "/api/devices/", token, "/"]
        uri = host gw ++ path

lookupUser :: Config -> ServiceList -> String -> String -> IO (Maybe User)
lookupUser Config {..} ref key token =
  if admin == key then
    if adminPassword == token then pure $ Just SuperAdmin
                              else pure Nothing
  else do
    srvs <- readTVarIO ref
    case getService srvs key of
      Nothing -> pure Nothing
      Just Service {..} ->
        if password == token then pure $ Just $ Admin $ MountPoint $ '/' : key
                             else do
          u <- try $ getUUID endpoint token
          case u of
            Left e -> Log.errorM "Lib" (errMsg e) >> pure Nothing
            Right (OkResult (UUID u0)) ->
              pure $ Just $ Normal $ MountPoint $ "/" ++ key ++ "/" ++ u0

authReqHandler :: Config -> ServiceList -> ActionM ()
authReqHandler config ref = do
  key <- param "username"
  token <- param "password"
  r <- liftIO $ lookupUser config ref key token
  case r of
    Nothing -> do
      status status400
      json $ object [ "err" .= ("no auth" :: String)]
    Just u -> json u

superReqHandler :: ActionM ()
superReqHandler = text "ok"

aclReqHandler :: ActionM ()
aclReqHandler = text "ok"

configReloadHandler :: Config -> ServiceList -> ActionM ()
configReloadHandler Config {..} ref = do
  liftIO $ loadService servicePath ref
  text "ok"

application :: Config -> ServiceList -> ScottyM ()
application config ref = do
  post "/mqtt/acl" aclReqHandler
  post "/mqtt/superuser" superReqHandler
  post "/mqtt/auth" $ authReqHandler config ref
  post "/config/reload" $ configReloadHandler config ref

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
      ref <- newTVarIO []
      loadService servicePath ref
      scottyOpts opts $ application conf ref
