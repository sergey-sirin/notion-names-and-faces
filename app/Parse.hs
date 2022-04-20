{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parse where

import Data.List (zipWith5)
import Data.Text (Text)
import Json

data Person = Person
  { name :: Text,
    telegram :: Maybe Text,
    avatarUri :: Maybe Text,
    personId :: Text,
    bio :: Maybe Text
  }

maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead [] = Nothing

getPersons :: QueryResult -> [Person]
getPersons z = do
  let base = properties <$> results z
  let names = plain_text . head . title . Json.name <$> base
  let telegrams = fmap plain_text . maybeHead . t_rich_text . Json.telegram <$> base
  let avatarUris = fmap (url . file) . maybeHead . files . photo <$> base
  let personIds = pid <$> results z
  zipWith5 Person names telegrams avatarUris personIds (repeat Nothing)

getText :: QueryResult -> [Text]
getText = fmap getBlockText . results

getBlockText :: Result -> Text
getBlockText EmptyBlock = mempty
getBlockText (Block {paragraph}) = mconcat . fmap plain_text . rich_text $ paragraph
getBlockText (Page _ _) = error "Only works on blocks!"
