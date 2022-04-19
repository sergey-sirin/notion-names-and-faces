{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), (.:))
import GHC.Generics

data QueryResult = QueryResult
  { results :: [Result]
  }
  deriving (Generic, Show, FromJSON)

data Result = Result
  { properties :: Properties
  }
  deriving (Generic, Show, FromJSON)

data Properties = Properties
  { name :: Name
  }
  deriving (Generic, Show)

instance FromJSON Properties where
  parseJSON (Object v) = Properties <$> v .: "Name"
  parseJSON _ = mzero

data Name = Name
  { title :: [NotionText]
  }
  deriving (Generic, Show, FromJSON)

data Telegram = Telegram
  { rich_text :: [NotionText]
  }
  deriving (Generic, Show, FromJSON)

data NotionText = NotionText
  { plain_text :: String
  }
  deriving (Generic, Show, FromJSON)
