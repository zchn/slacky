module Slack.API.RTM.Start where

import Control.Lens
import Slacky.Prelude

import Network.Wreq

type ApiToken = Text

-- | Implement me!
rtmStart :: ApiToken -> IO (Response LByteString)
rtmStart token = undefined

-- | @setParam key val opts@ modifies @opts@ by setting param @key@ to @val@.
setParam :: Text -> Text -> Options -> Options
setParam key val = set (param key) [val]