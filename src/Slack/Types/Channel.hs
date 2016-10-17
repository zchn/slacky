{-# LANGUAGE DeriveGeneric #-}

module Slack.Types.Channel where

import Slacky.Prelude
-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson
import GHC.Generics (Generic)

type ChannelId = Text

type ChannelName = Text

data RawChannel = RawChannel
    { id :: ChannelId
    , name :: ChannelName
    } deriving (Generic,Show)

instance FromJSON RawChannel

data Channel = Channel
    { channelId :: ChannelId
    , channelName :: ChannelName
    } deriving (Eq,Show)

instance FromJSON Channel where
    parseJSON v = do
        raw <- parseJSON v
        return $ Channel (Slack.Types.Channel.id raw) (name raw)
