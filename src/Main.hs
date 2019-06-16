{-# LANGUAGE OverloadedStrings
             , DeriveGeneric
             , FlexibleInstances
             , MultiParamTypeClasses
             , DataKinds
             , TypeOperators #-}

module Main where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           GHC.Generics
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           System.Environment

data M2H = M2H {
  m2hHours :: Integer
  , m2hMinutes :: Integer
  , m2hInitial :: Integer
  } deriving (Show, Read, Generic)

instance ToJSON M2H where
  toJSON (M2H h m b) =
    object ["hours" .= h
           , "minutes" .= m
           , "initial" .= b]

instance FromJSON M2H where
  parseJSON = withObject "M2H" $ \v -> M2H
        <$> v .: "hours"
        <*> v .: "minutes"
        <*> v .: "initial"

instance MimeRender PlainText M2H where
  mimeRender _ val = BSLC.pack $ "In "
    <> show (m2hInitial val) <> " minutes there are "
    <> show (m2hHours val) <> " hour(s) and " <> show (m2hMinutes val) <> " minute(s) ago."

type API = Capture "minutes" Integer :> Get '[JSON, PlainText] M2H

srv :: Server API
srv = return . m2out
  where
    m2out m = M2H (m2h m) (m2m m) m
    m2h = (`div` m)
    m2m = (`mod` m)
    m = 60

pApi :: Proxy API
pApi = Proxy

app :: Application
app = serve pApi srv

convert' :: Integer -> ClientM M2H
convert' = client pApi

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nb] -> do
      t <- forkIO $ run 8383 app
      manager' <- newManager defaultManagerSettings
      res <- runClientM (convert' (read nb::Integer)) (mkClientEnv manager' (BaseUrl Http "localhost" 8383 ""))
      case res of
        Right m -> putStrLn $ show (m2hHours m) <> " hour(s) " <> show (m2hMinutes m) <> " minute(s)"
        Left err -> print err
      killThread t
    _ -> putStrLn "usage: m2h <minutes>"
