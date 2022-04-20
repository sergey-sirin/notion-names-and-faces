{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Lens
import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Json (QueryResult)
import Lucid
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wreq qualified as W (Options, defaults, getWith, header, postWith)
import Network.Wreq.Lens (responseBody)
import Parse
import System.Environment (getEnv)

data State = State
  { notionApiToken :: ByteString,
    persons :: [Person]
  }

fetchPersonText :: W.Options -> Person -> IO (Maybe [T.Text])
fetchPersonText opts (Person {personId}) = do
  r' <- W.getWith opts $ "https://api.notion.com/v1/blocks/" <> T.unpack personId <> "/children"
  T.putStrLn personId
  let z = Aeson.eitherDecode @QueryResult $ r' ^. responseBody
  case z of
    Left _ -> print personId >> print z >> pure mempty
    Right x -> pure $ getText x

main :: IO ()
main = do
  notionApiToken <- B8.pack <$> getEnv "NOTION_API_TOKEN"

  let opts =
        W.defaults
          & W.header "Authorization" .~ [notionApiToken]
          & W.header "Notion-Version" .~ ["2022-02-22"]
  r <- W.postWith opts "https://api.notion.com/v1/databases/67738a4428bb40c08968d9ce261342bf/query" (mempty :: ByteString)
  let z = Aeson.eitherDecode @QueryResult $ r ^. responseBody
  let Right zz = z
  let persons_ = getPersons zz

  texts <- mapConcurrently (fetchPersonText opts) $ take 5 persons_
  let persons = zipWith (\p t -> p {bio = t}) persons_ $ (fmap . fmap) T.unwords texts

  run 3000 (app $ State {..})

app :: State -> Application
app (State {..}) _ respond =
  respond $ responseLBS status200 [] $ renderBS (html persons)

html :: [Person] -> Html ()
html _data =
  doctypehtml_ $ do
    head_ $
      meta_ [charset_ "utf-8"]
    body_ $
      div_ [style_ "display: flex; flex-direction: row; flex-wrap: wrap;"] $
        forM_ _data personCard

personCard :: Person -> Html ()
personCard _data =
  section_ [style_ "transform: rotate(4deg); width: 300px;"] $ do
    img_ [src_ $ fromMaybe "https://i.redd.it/vb4e2jl0pfz21.jpg" $ avatarUri _data, style_ "width: 100px; height: 100px; object-fit: cover;"]
    div_ . toHtml $ name _data
    div_ . toHtml $ fromMaybe "no telegrammzz(" $ telegram _data
    let tarakan =
          mconcat . replicate 3 . T.unwords $
            [ "я весёлый таракан",
              "я бегу бегу я весёлый",
              "таракан я бегу бегу",
              "нефига ты не попал",
              "я все равно убежал",
              "я весёлый таракан",
              "я бегу бегу",
              "все равно я убежал"
            ]
    div_ . toHtml $ fromMaybe tarakan $ bio _data
