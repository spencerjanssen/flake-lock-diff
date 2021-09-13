{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import Data.Map.Merge.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.Clock.POSIX
import Relude.Extra.Map
import System.Environment

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

newtype NarHash = NarHash Text
    deriving (Show)
    deriving newtype (FromJSON)

data Lock = Lock
    { lastModified :: POSIXTimestamp
    , narHash :: NarHash
    }
    deriving (Generic, Show)
    deriving anyclass (FromJSON)

data Source
    = Stdin
    | File FilePath

main :: IO ()
main = do
    as <- getArgs
    case as of
        [x] -> main' Stdin (File x)
        [x, y] -> main' (File x) (File y)
        _ ->
            fail $
                toString $
                    unlines
                        [ "Usage: flake-lock-diff old_lock_file current_lock_file"
                        , "OR to get old_lock_file from stdin"
                        , "flake-lock-diff current_lock_file"
                        ]

eitherDecodeSourceStrict :: FromJSON a => Source -> IO (Either String a)
eitherDecodeSourceStrict Stdin = eitherDecodeStrict <$> B.getContents
eitherDecodeSourceStrict (File f) = eitherDecodeFileStrict f

main' :: Source -> Source -> IO ()
main' old new = do
    flo <- either fail pure =<< eitherDecodeSourceStrict @FlakeLock old
    fl <- either fail pure =<< eitherDecodeSourceStrict @FlakeLock new
    let mt = inputModTimes fl
        mto = inputModTimes flo
        d = differences mto mt
    TIO.putStrLn $ commitMessage d

type LockDesc = Map Text UTCTime

type LockDiff = Map Text Change

data Change
    = Added UTCTime
    | Removed
    | Updated UTCTime UTCTime
    deriving (Show)

inputModTimes :: FlakeLock -> LockDesc
inputModTimes FlakeLock{nodes, root} = fromList do
    Node{inputs = Just is} <- getNode root
    (LocalRef inputName, ToNode inputRef) <- toPairs is
    Node{locked = Just Lock{lastModified = POSIXTimestamp t}} <- getNode inputRef
    pure (inputName, t)
  where
    getNode x = maybeToList $ lookup x nodes

differences :: LockDesc -> LockDesc -> LockDiff
differences = merge inl inr inb
  where
    inl = Removed <$ preserveMissing
    inr = Added <$> preserveMissing
    inb = zipWithMaybeMatched (const f)
    f l r
        | l == r = Nothing
        | otherwise = Just $ Updated l r

commitMessage :: LockDiff -> Text
commitMessage d =
    unlines $
        [ case differenceShortDescription d of
            [] -> "Unknown changes"
            xs -> T.intercalate "; " xs
        , ""
        ]
            <> differenceLongDescription d

differenceShortDescription :: LockDiff -> [Text]
differenceShortDescription d =
    mapMaybe
        (uncurry listFor)
        [ ("Add", adds)
        , ("Remove", rems)
        , ("Update", upds)
        ]
  where
    adds = [i | (i, Added _) <- toPairs d]
    rems = [i | (i, Removed) <- toPairs d]
    upds = [i | (i, Updated _ _) <- toPairs d]
    listFor _ [] = Nothing
    listFor description xs = Just $ description <> " " <> T.intercalate ", " xs

differenceLongDescription :: LockDiff -> [Text]
differenceLongDescription = map f . toPairs
  where
    f (d, Added x) = d <> ": " <> "init @" <> show x
    f (d, Removed) = d <> " removed"
    f (d, Updated old new) = d <> ": " <> show old <> " -> " <> show new
