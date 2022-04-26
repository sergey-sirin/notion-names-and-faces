{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use underscore" #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, mapConcurrently_)
import Control.Lens
import Control.Monad (forM_, forever)
import Control.Monad.STM
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Json (QueryResult)
import ListT qualified
import Lucid
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wreq qualified as W (Options, defaults, getWith, header, postWith)
import Network.Wreq.Lens (responseBody)
import Parse
import StmContainers.Set qualified as Set
import System.Environment (getEnv)

data State = State
    { notionApiToken :: ByteString
    , persons :: Set.Set Person
    }

fetchPersonText :: W.Options -> Person -> IO (Maybe [T.Text])
fetchPersonText opts (Person{personId}) = do
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

    persons <- Set.newIO

    let augmentPerson seedPerson = do
            t <- fetchPersonText opts seedPerson
            pure $ seedPerson{bio = T.unwords <$> t}
    let augmentAndUpsert seedPerson = do
            p <- augmentPerson seedPerson
            atomically $ do
                -- XXX: upsert w/focus?
                Set.delete p persons
                Set.insert p persons

    concurrently_
        (run 3000 (app $ State{..}))
        ( forever $ do
            mapConcurrently_ augmentAndUpsert persons_
            threadDelay 300_000_000
        )

app :: State -> Application
app (State{..}) _ respond = do
    persons' <- atomically $ ListT.toList $ Set.listT persons
    respond $ responseLBS status200 [] $ renderBS (html persons')

html :: [Person] -> Html ()
html _data =
    doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "user-scalable=no, width=device-width, initial-scale=0.5"]
            link_ [rel_ "stylesheet", type_ "text/css", href_ "index.css"]
            script_ [src_ "index.js", defer_ ""] (mempty :: ByteString)
        body_ $
            div_ [class_ "cardscontainer"] $
                forM_ _data personCard

personCard :: Person -> Html ()
personCard _data =
    section_
        [ class_ "personcard"
        , onclick_ $
            "popup(`"
                <> name _data
                <> "`, `"
                <> fromMaybe "" (bio _data)
                <> "`)"
        ]
        $ do
            img_ [src_ $ fromMaybe "https://i.redd.it/vb4e2jl0pfz21.jpg" $ avatarUri _data]
