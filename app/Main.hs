{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.Time
import Data.Time.Clock.POSIX
import Relude.Extra.Map

newtype NodeRef = NodeRef Text
    deriving stock (Show)
    deriving newtype (Eq, Ord, FromJSON, FromJSONKey)

newtype LocalRef = LocalRef Text
    deriving stock (Show)
    deriving newtype (Eq, Ord, FromJSON, FromJSONKey)

data FlakeLock = FlakeLock
    { nodes :: Map NodeRef Node
    , root :: NodeRef
    }
    deriving (Generic, Show)
    deriving anyclass (FromJSON)

data InputRef
    = ToNode NodeRef
    | WeirdArrayThing
    deriving (Show)

instance FromJSON InputRef where
    parseJSON r@(String _) = ToNode <$> parseJSON r
    parseJSON (Array _) = pure WeirdArrayThing
    parseJSON x =
        prependFailure
            "parsing InputRef failed, "
            (typeMismatch "String or Array" x)

data Node = Node
    { inputs :: Maybe (Map LocalRef InputRef)
    , locked :: Maybe Lock
    }
    deriving (Generic, Show)
    deriving anyclass (FromJSON)

newtype POSIXTimestamp = POSIXTimestamp UTCTime
    deriving (Show)

instance FromJSON POSIXTimestamp where
    parseJSON x = prependFailure "parsing POSIXTimestamp failed, " $ do
        POSIXTimestamp . posixSecondsToUTCTime . fromIntegral <$> parseJSON @Int x

data Lock = Lock
    { lastModified :: POSIXTimestamp
    , narHash :: Text
    }
    deriving (Generic, Show)
    deriving anyclass (FromJSON)

main :: IO ()
main = do
    fl <- either fail pure =<< eitherDecodeFileStrict @FlakeLock "flake.lock"
    print $ inputModTimes fl

inputModTimes :: FlakeLock -> [(Text, UTCTime)]
inputModTimes FlakeLock{nodes, root} = do
    Node{inputs = Just is} <- getNode root
    (LocalRef inputName, ToNode inputRef) <- toPairs is
    Node{locked = Just Lock{lastModified = POSIXTimestamp t}} <- getNode inputRef
    pure (inputName, t)
  where
    getNode x = maybeToList $ lookup x nodes
