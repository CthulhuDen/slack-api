{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Web.Slack.Types.Time where

import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Control.Applicative
import Control.Error
import Control.Lens.TH

default ()

newtype Time = Time { _getTime :: POSIXTime } deriving (Fractional, Num, Real, Eq, Ord, Show)

-- Might be better to keep this abstract..
data SlackTimeStamp = SlackTimeStamp { _slackTime :: Time, _timestampUid :: Int } deriving (Show, Ord, Eq)

makeLenses ''SlackTimeStamp
makeLenses ''Time

instance FromJSON SlackTimeStamp where
  parseJSON = withText "SlackTimeStamp"
                (\s -> let (ts, tail -> uid) = break (== '.') (T.unpack s) in
                  SlackTimeStamp
                    <$> parseTimeString ts
                    <*> readZ uid)

instance ToJSON SlackTimeStamp where
  toJSON (SlackTimeStamp t u) = toJSON (timeToString t ++ "." ++ show u :: String)

instance FromJSON Time where
  parseJSON (Number s) = return $ Time $ realToFrac s
  parseJSON (String t) = parseTimeString $ T.unpack t
  parseJSON _ = empty

parseTimeString :: String -> Parser Time
parseTimeString s = fmap (Time . realToFrac) (readZ s :: Parser Integer)

timeToString :: Time -> String
timeToString (Time t) = show (round t :: Int)
