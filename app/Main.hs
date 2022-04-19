{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket_)
import Control.Lens
import Control.Monad (replicateM_)
import Data.Aeson qualified as Aeson (encode)
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Lucid
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wreq
import Network.Wreq.Lens (responseBody)
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
        let x =
              r
                ^. responseBody
                  . key "results"
                  . _Array
                  . traverse
                  . key "properties"
                  . key "Name"
                  . key "title"
                  . nth 0
                  . key "plain_text"
                  . _String
        respond $ responseLBS status200 [] $ Aeson.encode x
    )

html :: Html ()
html = doctypehtml_ $
  div_ [style_ "display: flex;"] $
    replicateM_ 4 $
      section_ $ do
        img_ [src_ "./face.jpg", style_ "width: 100px;"]
        div_ "a"
        div_ "b"
        div_ "c"
