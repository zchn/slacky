module Main where

import Slack.API.RTM.Start
import Slack.Types.RTM.Start
import Slacky.Lifted
import Slacky.Monad
import Slacky.Prelude

-- import Data.Maybe (maybe)
import Control.Exception (SomeException, displayException, throwIO)
import Control.Lens (view)
import Control.Monad (when)
import Data.Aeson (decode)
import Data.Default.Class (def)
import System.Environment (getEnv)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO.Error (catchIOError, isDoesNotExistError, isEOFError)

import qualified Data.ByteString.Lazy as DBL
import qualified Network.Connection as NC
-- (ConnectionParams(ConnectionParams, connectionHostname, connectionPort, connectionUseSecure, connectionUseSocks), connectionClose, connectTo, initConnectionContext, connectionGetChunk, connectionPut)
import qualified Network.Wreq as Wreq
import qualified Network.WebSockets as NW
import qualified Network.WebSockets.Stream as NWS

-- Run 'slackyMain' with the desired logging method (to stdout or to stderr) and
-- the desired threshold (Debug).
main
    :: IO ()
main = runSlackyStderr Debug slackyMain

-- Port the 'oldMain' code to the 'Slacky' monad by peppering all function calls
-- in the IO monad with 'io' (exported by Slacky.Prelude). Then, replace the
-- existing explicit writes to stderr to use the logging API. Then, add some
-- more log messages.
slackyMain
    :: Slacky ()
slackyMain = do
    logWarning "Just some one time warning."
    logDebug "Just some one time debug message."
    logInfo "Just some one time info message."
    token <-
        do (io . getEnv') "SLACK_API_TOKEN" >>=
               \case
                   Nothing -> do
                       logError "Missing required env variable SLACK_API_TOKEN"
                       io $ exitWith (ExitFailure 1)
                   Just x -> pure x
    -- Tricky!
    response <-
        liftedCatch
            (io $ rtmStart (pack token))
            (\e ->
                  do logError $ pack (displayException (e :: SomeException))
                     io $ exitWith (ExitFailure 1))
    let code = responseStatusCode response
    when (code /= 200) $
        do logError $ pack $ show code
           io $ exitWith (ExitFailure 1)
    let body = responseBody response

    -- I re-structured this a bit from last time, to help with indentation creep.
    -- The record wildcard pulls things like rtmStartHost, rtmStartPath, etc. into
    -- top-level scope. That is,
    --
    --   rtmStartHost :: String
    --
    -- in this context, even though normally it's a projection function with type
    --
    --   rtmStartHost :: RTMStart -> String
    --
    RTMStart{
      rtmStartHost = rtmHost,
      rtmStartPath = rtmPath
            } <-
      case decode body of
        Nothing -> do
          logError "Could not decode response body:"
          logError $ pack $ show body
          io $ exitWith (ExitFailure 1)
        Just val -> pure val
    logDebug (pack $ show rtmHost)
    logDebug (pack $ show rtmPath)

    -- Initialize a connection context
    context <- io NC.initConnectionContext

    -- Connect to the Slack host on port 443 over TLS. Remember to clean up the
    -- collection in an exception-safe way! (You'll need to implement
    -- 'liftedBracket' for this. Head over to src/Slacky/Lifted.hs).
    liftedBracket (io $ NC.connectTo context $ NC.ConnectionParams {
                      NC.connectionHostname = rtmHost,
                        NC.connectionPort = 443,
                        NC.connectionUseSecure = Just def,
                        NC.connectionUseSocks = Nothing
                      }) (io . NC.connectionClose) (
      \connection -> do
        let readConn = fmap Just (NC.connectionGetChunk connection)
              `catchIOError` \e ->
                               if isEOFError e then pure Nothing else throwIO e
            writeConn = \case
              Nothing -> pure ()
              Just bytes -> NC.connectionPut connection (DBL.toStrict bytes)
        liftedBracket
          (io (NWS.makeStream readConn writeConn))
          (io . NWS.close)
          (\stream -> liftedRunClientWithStream stream rtmHost rtmPath
            NW.defaultConnectionOptions [] client))

client :: NW.Connection -> Slacky ()
client conn = do
  -- Loop forever, printing out (as Text) every message received.
  bytes <- io (NW.receiveData conn)
  logDebug (format "Received bytes: {}" (Only (bytes :: Text)))
  client conn

-- | Like 'getEnv' from System.Environment, but instead of throwing a
-- synchronous exception when the environment variable is not found, return
-- Nothing.
getEnv'
    :: String -> IO (Maybe String)
getEnv' name =
    catchIOError
        (Just <$> getEnv name)
        (\e ->
              if isDoesNotExistError e
                  then pure Nothing
                  else ioError e)

responseStatusCode :: Wreq.Response LByteString -> Int
responseStatusCode = view (Wreq.responseStatus . Wreq.statusCode)

responseBody :: Wreq.Response LByteString -> LByteString
responseBody = view Wreq.responseBody
