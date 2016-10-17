module Slacky.Monad
       (Slacky, unliftIO, runSlackyStdout, runSlackyStderr, Severity(..),
        logDebug, logInfo, logWarning, logError)
       where

import Slacky.Prelude
-- https://www.stackage.org/lts-6.11/package/base
import Control.Monad (ap)
import System.IO (stderr, stdout, Handle)
-- https://www.stackage.org/lts-6.11/package/transformers
import Control.Monad.IO.Class (MonadIO, liftIO)
-- https://www.stackage.org/lts-6.11/package/text
import Data.Text.Lazy.IO (hPutStrLn)

-- | The severity of the message being logged.
data Severity
    = Debug 
    | Info 
    | Warning 
    | Error 
    deriving (Eq,Ord,Show)

type LogFunction = Severity -> LText -> IO ()

newtype Slacky a = Slacky
    { runSlacky :: LogFunction -> IO a
    } 

instance Functor Slacky where
    fmap :: (a -> b) -> Slacky a -> Slacky b
    fmap f (Slacky runner) = Slacky ((fmap f) . runner)

instance Applicative Slacky where
    pure :: a -> Slacky a
    pure = return
    (<*>) :: Slacky (a -> b) -> Slacky a -> Slacky b
    (<*>) = ap

instance Monad Slacky where
    return :: a -> Slacky a
    return a = 
        Slacky
            (\_ -> 
                  return a)
    (>>=) :: Slacky a -> (a -> Slacky b) -> Slacky b
    (>>=) sa f = Slacky runnerB
      where
        runnerB lf = do
            av <- runSlacky sa lf
            let sb = f av
            runSlacky sb lf

instance MonadIO Slacky where
    liftIO :: IO a -> Slacky a
    liftIO a = 
        Slacky
            (\_ -> 
                  a)

-- | In the 'Slacky' monad, return a function that can unlift other 'Slacky'
-- computations to IO.
unliftIO
    :: Slacky (Slacky a -> IO a)
unliftIO = 
    Slacky
        (\logf -> 
              pure
                  (\sa -> 
                        runSlacky sa logf))

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stdout.
runSlackyStdout
    :: Severity -> Slacky a -> IO a
runSlackyStdout = runSlackyHandle stdout

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stderr.
runSlackyStderr
    :: Severity -> Slacky a -> IO a
runSlackyStderr = runSlackyHandle stderr

runSlackyHandle :: Handle -> Severity -> Slacky a -> IO a
runSlackyHandle handle severity sa = runSlacky sa lf
  where
    lf sev txt = 
        if sev >= severity
            then hPutStrLn handle txt
            else return ()

logStr :: Severity -> LText -> Slacky ()
logStr severity text = 
    Slacky
        (\lf -> 
              lf severity text)

-- | Log a debug message.
logDebug
    :: LText -> Slacky ()
logDebug = logStr Debug

-- | Log an info message.
logInfo
    :: LText -> Slacky ()
logInfo = logStr Info

-- | Log a warning message.
logWarning
    :: LText -> Slacky ()
logWarning = logStr Warning

-- | Log an error message.
logError
    :: LText -> Slacky ()
logError = logStr Error
