{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.ByteString.Char8 as B
import           Data.Text as T
import           Text.Read as R
import qualified Text.Blaze.Html5 as H
import           Snap.Blaze

factorial :: Integer -> Integer
factorial n = product [1..n]

readNumber :: Maybe ByteString -> Maybe Integer
readNumber mn = fmap B.unpack mn >>= R.readMaybe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    route [ ("factorial/:number" , handleFactorial )
          , ("factorial1/:number", handleFactorial )
          , ("factorial2/:number", handleFactorial2)
          , ("factorial3/:number", handleFactorial3)
          , ("factorial4/:number", handleFactorial4)
          ] <|>
    dir "static" (serveDirectory ".")

-- | Simply text
handleFactorial :: Snap ()
handleFactorial = do
    mbNumber <- getParam "number"
    maybe (writeBS "must specify factorial/number in URL")
          (\n -> writeBS $ showf n $ factorial n)
          (readNumber mbNumber)
  where showf n f = B.pack $ "factorial " ++ show n ++ " = " ++ show f

-- | Html text
handleFactorial2 :: Snap ()
handleFactorial2 = do
    mbNumber <- getParam "number"
    blaze $
      maybe
          (H.text "must specify factorial/number in URL")
          (\n -> H.text $ showf n $ factorial n)
          (readNumber mbNumber)
  where showf n f = T.pack $ "factorial " ++ show n ++ " = " ++ show f

-- | With proper page tags
handleFactorial3 :: Snap ()
handleFactorial3 = do
    mbNumber <- getParam "number"
    blaze $ H.html $ do
      H.head $ H.title "Factorial" 
      H.body $ case readNumber mbNumber of
          Just n -> H.text $ showf n $ factorial n
          _ -> H.text "must specify factorial/number in URL"
  where showf n f = T.pack $ "factorial " ++ show n ++ " = " ++ show f

-- | More fancy html
handleFactorial4 :: Snap ()
handleFactorial4 = do
    mbNumber <- getParam "number"
    blaze $ H.html $ do
      H.head $ H.title "Factorial" 
      H.body $ case readNumber mbNumber of
          Just n -> do
            H.hr
            H.table H.! H.customAttribute "border" "1" $ do
              H.tr $ do
                H.th "Number"
                H.th "Factorial"
              H.tr $ do
                H.td $ H.toHtml n
                H.td $ H.toHtml $ factorial n
            H.hr
          _ -> H.text "must specify factorial/number in URL"

