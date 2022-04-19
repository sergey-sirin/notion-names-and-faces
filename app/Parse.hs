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
    telegram :: B8.ByteString
  }

maybeHead (x : _) = Just x
maybeHead [] = Nothing

getPersons :: QueryResult -> [Person]
getPersons z = do
  let names = T.pack . plain_text . head . title . Json.name . properties <$> results z
  let telegrams = maybe "" (B8.pack . plain_text) . maybeHead . rich_text . Json.telegram . properties <$> results z
  zipWith Person names telegrams
