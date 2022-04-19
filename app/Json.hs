{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Json where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), (.:))
import Data.Text (Text)
import GHC.Generics

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

data QueryResult = QueryResult
  { results :: [Result]
  }
  deriving (Generic, Show, FromJSON)

data Result = Result
  { properties :: Properties
  }
  deriving (Generic, Show, FromJSON)

data Properties = Properties
  { name :: Name,
    telegram :: Telegram,
    photo :: Photo
  }
  deriving (Generic, Show)

instance FromJSON Properties where
  parseJSON (Object v) =
    Properties
      <$> v .: "Name"
      <*> v .: "Telegram"
      <*> v .: "Фото"
  parseJSON _ = mzero

data Name = Name
  { title :: [NotionText]
  }
  deriving (Generic, Show, FromJSON)

data Telegram = Telegram
  { rich_text :: [NotionText]
  }
  deriving (Generic, Show, FromJSON)

data Photo = Photo
  { files :: [File]
  }
  deriving (Generic, Show, FromJSON)

data File = File
  { file :: File'
  }
  deriving (Generic, Show, FromJSON)

data File' = File'
  { url :: Text
  }
  deriving (Generic, Show, FromJSON)

data NotionText = NotionText
  { plain_text :: Text
  }
  deriving (Generic, Show, FromJSON)
