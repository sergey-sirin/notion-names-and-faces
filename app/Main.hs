{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Main where

import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Lucid
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Exception (bracket_)
import Network.HTTP.Types (status200)

main :: IO ()
main = run 3000 app

app :: Application
app req respond = bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    (respond $ responseLBS status200 [] $ renderBS html)

html :: Html ()
html = doctypehtml_ $ section_ $ do
    img_ [ src_ "./face.jpg", style_ "width: 100px;" ]
    div_ "a"
    div_ "b"
    div_ "c"