module Types
  ( ZettelID (..),
    Zettel (..),
    Connection (..)
  )
  where

import Data.Text

newtype ZettelID = ZID { getZID :: Int } deriving (Show, Eq)

data Zettel = Zettel {
                getId :: ZettelID,
                getTimestamp :: Text,
                getTitle :: Text,
                getAuthors :: [Text],
                getZettel :: Text,
                getTags :: [Text],
                getConnections :: [Connection]
              } deriving (Show, Eq)

data Connection = Connection {
                    getCID :: ZettelID,
                    getDesc :: Text
                  } deriving (Show, Eq)
