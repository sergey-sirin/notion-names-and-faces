{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parse where

import Data.ByteString.Char8 qualified as B8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Json

data Person = Person
  { name :: Text,
    telegram :: Text,
    avatarUri :: Text
  }

maybeHead (x : _) = Just x
maybeHead [] = Nothing

getPersons :: QueryResult -> [Person]
getPersons z = do
  let base = properties <$> results z
  let names = plain_text . head . title . Json.name <$> base
  let telegrams = maybe "" plain_text . maybeHead . rich_text . Json.telegram <$> base
  let avatarUris = maybe "" (url . file) . maybeHead . files . photo <$> base
  zipWith3 Person names telegrams avatarUris
