{-# LANGUAGE ImportQualifiedPost #-}

module Parse where

import Data.ByteString.Char8 qualified as B8
import Data.Text (Text)
import Data.Text qualified as T
import Json

data Person = Person
  { name :: Text
  }

getPersons :: QueryResult -> [Person]
getPersons z = do
  let names = T.pack . plain_text . head . title . Json.name . properties <$> results z
  map Person names
