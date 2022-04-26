{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parse where

import Data.Hashable
import Data.List (zipWith5)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Json

data Person = Person
    { name :: Text
    , telegram :: Maybe Text
    , avatarUri :: Maybe Text
    , personId :: Text
    , bio :: Maybe Text
    }
    deriving (Generic)

instance Eq Person where
    (Person{personId = personId1}) == (Person{personId = personId2}) = personId1 == personId2

instance Hashable Person where
    hashWithSalt salt (Person{personId}) = hashWithSalt salt personId

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

listToMaybe' :: [a] -> Maybe [a]
listToMaybe' [] = Nothing
listToMaybe' x = Just x

getText :: QueryResult -> Maybe [Text]
getText = listToMaybe' . mapMaybe getBlockText . results

getBlockText :: Result -> Maybe Text
getBlockText EmptyBlock = Nothing
getBlockText (Block{paragraph}) = Just $ mconcat . fmap plain_text . rich_text $ paragraph
getBlockText (Page _ _) = Nothing
