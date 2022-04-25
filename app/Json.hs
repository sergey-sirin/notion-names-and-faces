{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Json where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value (Object), (.:))
import Data.Foldable (asum)
import Data.Text (Text)
import GHC.Generics

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

data QueryResult = QueryResult
    { results :: [Result]
    }
    deriving (Generic, Show, FromJSON)

data Result
    = Page
        { properties :: Properties
        , pid :: Text
        }
    | Block
        { paragraph :: Paragraph
        , _type :: Text
        }
    | EmptyBlock
    deriving (Generic, Show)

instance FromJSON Result where
    parseJSON (Object v) =
        asum
            [ Page
                <$> v .: "properties"
                <*> v .: "id"
            , Block
                <$> v .: "paragraph"
                <*> v .: "type"
            , pure EmptyBlock
            ]
    parseJSON _ = mzero

data Properties = Properties
    { name :: Name
    , telegram :: Telegram
    , photo :: Photo
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
    { t_rich_text :: [NotionText]
    }
    deriving (Generic, Show)

instance FromJSON Telegram where
    parseJSON (Object v) =
        Telegram
            <$> v .: "rich_text"
    parseJSON _ = mzero

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

data Paragraph = Paragraph
    { rich_text :: [NotionText]
    }
    deriving (Generic, Show, FromJSON)

data NotionText = NotionText
    { plain_text :: Text
    }
    deriving (Generic, Show, FromJSON)
