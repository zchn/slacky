module Slacky.Prelude
  ( DT.Text
  , LText
  , DB.ByteString
  , LByteString
  , implementMe
  , pack
  , unpack
  , io
  , Only(..)
  , format
  ) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text.Format (Only(..), format)
import qualified Data.ByteString         as DB
import qualified Data.ByteString.Lazy    as DBL
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL

type LByteString = DBL.ByteString

type LText = DTL.Text

implementMe :: a
implementMe = error "Implement me!"

class Pack a  where
    pack :: String -> a

instance Pack DT.Text where
    pack = DT.pack

instance Pack DTL.Text where
    pack = DTL.pack

class Unpack a  where
    unpack :: a -> String

instance Unpack DT.Text where
    unpack = DT.unpack

io
    :: MonadIO m
    => IO a -> m a
io = liftIO
