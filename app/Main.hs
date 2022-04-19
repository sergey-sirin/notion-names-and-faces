{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (bracket_)
import Control.Lens
import Control.Monad (forM_, replicateM_)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Json (QueryResult)
import Lucid
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wreq (defaults, header, postWith, responseBody)
import Network.Wreq.Lens (responseBody)
import Parse
import System.Environment (getEnv)

main :: IO ()
main = do
  notionApiToken <- B8.pack <$> getEnv "NOTION_API_TOKEN"
  run 3000 (app notionApiToken)

app :: ByteString -> Application
app notionApiToken req respond =
  bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    ( do
        let opts =
              defaults
                & header "Authorization" .~ [notionApiToken]
                & header "Notion-Version" .~ ["2022-02-22"]
        r <- postWith opts "https://api.notion.com/v1/databases/67738a4428bb40c08968d9ce261342bf/query" (mempty :: ByteString)
        let Just z = Aeson.decode @QueryResult $ r ^. responseBody
        respond $ responseLBS status200 [] $ renderBS (html $ getPersons z)
    )

html :: [Person] -> Html ()
html _data =
  doctypehtml_ $ do
    head_ $
      meta_ [charset_ "utf-8"]
    body_ $
      div_ [style_ "display: flex;"] $
        forM_ _data personCard

personCard :: Person -> Html ()
personCard _data =
  section_ $ do
    img_ [src_ "./face.jpg", style_ "width: 100px;"]
    div_ . toHtml $ name _data
    div_ . toHtml $ telegram _data
