{-# LANGUAGE DeriveGeneric #-}

module Slack.Types.User where

import Slacky.Prelude
-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson
import GHC.Generics (Generic)

type UserId = Text

type UserName = Text

data RawUser = RawUser
    { id :: UserId
    , name :: UserName
    } deriving (Generic,Show)

instance FromJSON RawUser

data User = User
    { userId :: Text
    , userName :: Text
    } deriving (Eq,Show)

instance FromJSON User where
    parseJSON v = do
        raw <- parseJSON v
        return $ User (Slack.Types.User.id raw) (name raw)
