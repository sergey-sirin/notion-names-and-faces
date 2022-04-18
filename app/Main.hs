{-# LANGUAGE OverloadedStrings#-}

module Main where

import Data.Text.IO as T
import Data.Text.Lazy.IO as TL
import Lucid

main :: IO ()
main = TL.putStrLn $ renderText $ span_ "a"
