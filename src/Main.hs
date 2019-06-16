{-# LANGUAGE OverloadedStrings
             , DeriveGeneric
             , FlexibleInstances
             , MultiParamTypeClasses
             , DataKinds
             , TypeOperators #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

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

main :: IO ()
main = run 8081 app
