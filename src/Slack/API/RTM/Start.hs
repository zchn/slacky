module Slack.API.RTM.Start (rtmStart) where

import Network.Wreq (param, Options, getWith, defaults, Response)
import Control.Lens
import Slacky.Prelude

type ApiToken = Text

rtmStart :: ApiToken -> IO (Response LByteString)
rtmStart token = do
    -- xoxp-21444141684-21446588517-86267279605-5c129e9f529f48e064961c61d0d0f364
    let opts = 
            (setParam "token" token .
             setParam "simple_latest" "true" . setParam "no_unreads" "true")
                defaults
    r <- getWith opts "https://slack.com/api/rtm.start"
    return r

-- | @setParam key val opts@ modifies @opts@ by setting param @key@ to @val@.
setParam
    :: Text -> Text -> Options -> Options
setParam key val = set (param key) [val]
