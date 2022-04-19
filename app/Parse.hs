{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parse where

import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as B8
import Data.List (zipWith4)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Json
import Json (QueryResult (QueryResult))

data Person = Person
  { name :: Text,
    telegram :: Text,
    avatarUri :: Text,
    personId :: Text
  }

maybeHead (x : _) = Just x
maybeHead [] = Nothing

getPersons :: QueryResult -> [Person]
getPersons z = do
  let base = properties <$> results z
  let names = plain_text . head . title . Json.name <$> base
  let telegrams = maybe "" plain_text . maybeHead . t_rich_text . Json.telegram <$> base
  let avatarUris = maybe "" (url . file) . maybeHead . files . photo <$> base
  let personIds = pid <$> results z
  zipWith4 Person names telegrams avatarUris personIds

getText :: QueryResult -> [Text]
getText z = mconcat . fmap plain_text . rich_text . paragraph <$> results z
