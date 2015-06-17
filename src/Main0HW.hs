{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.ByteString.Char8 as B

factorial :: Int -> Int
factorial n = product [1..n]

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS $ B.pack $ "Hi World! You know what? The value of `factorial 5' " ++ (show $ factorial 5) ++ "!") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
