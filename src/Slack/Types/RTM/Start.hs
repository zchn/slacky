{-# LANGUAGE DeriveGeneric #-}

module Slack.Types.RTM.Start where


-- import Data.Maybe (isJust)
import Data.Aeson (FromJSON(parseJSON))
import Data.Aeson.Types (typeMismatch)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Network.URI
       (parseURI, URI, uriPath, uriAuthority, URIAuth, uriRegName)
import Slack.Types.Channel
import Slack.Types.IM
import Slack.Types.User
import Slacky.Prelude

data RTMStart = RTMStart
    { rtmStartHost :: String
    , rtmStartPath :: String
    , rtmStartUsers :: Vector User
    , rtmStartChannels :: Vector Channel
    , rtmStartGroups :: Vector Channel
    , rtmStartIMs :: Vector IM
    } deriving (Eq,Show)

data RawRTMStart = RawRTMStart
    { ok :: Bool
    , url :: String
    , users :: Vector User
    , channels :: Vector Channel
    , groups :: Vector Channel
    , ims :: Vector IM
    } deriving (Generic,Show)

instance FromJSON RawRTMStart

instance FromJSON RTMStart where
    parseJSON v = do
        raw <- parseJSON v
        case parseHostPath (pack $ url raw) of
            Just (host,path) ->
                return $
                RTMStart
                { rtmStartHost = host
                , rtmStartPath = path
                , rtmStartUsers = users raw
                , rtmStartChannels = channels raw
                , rtmStartGroups = groups raw
                , rtmStartIMs = ims raw
                }
            Nothing -> typeMismatch "RTMStart" v

-- Parse a host/path out of a Text.
--
-- Convert from a Text to a String using 'unpack', imported from Slacky.Prelude.
-- Then, use the parsing functions in Network.URI to dig out the host and path.
--
-- To test only this function:
--
--     stack test --fast --file-watch --test-arguments="-m Slack.Types.RTM.Start.parseHostPath"
--
parseHostPath
    :: Text -> Maybe (String, String)
parseHostPath url =
    let uri = parseURI (unpack url)
        path = maybe Nothing (Just . uriPath) uri
        auth = maybe Nothing uriAuthority uri
        host = maybe Nothing (Just . uriRegName) auth
    in case (host, path) of
           (Nothing,_) -> Nothing
           (_,Nothing) -> Nothing
           (Just h,Just p) -> Just (h, p)
