{-# LANGUAGE DeriveGeneric #-}

module Slack.Types.IM where

import Slack.Types.User (UserId)
import Slacky.Prelude
-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson
import GHC.Generics (Generic)

type IMId = Text

data RawIm = RawIm
    { id :: IMId
    , user :: UserId
    } deriving (Generic,Show)

instance FromJSON RawIm

data IM = IM
    { imId :: IMId
    , imUser :: UserId
    } deriving (Eq,Show)

instance FromJSON IM where
    parseJSON v = do
        raw <- parseJSON v
        return $ IM (Slack.Types.IM.id raw) (user raw)
