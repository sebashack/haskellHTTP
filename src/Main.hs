{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Types.Header (hUserAgent)
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S

parseStringAttr :: T.Text -> Object -> Maybe T.Text
parseStringAttr attr obj = do
  val <- parseMaybe (obj .:) attr
  case val of
    (String "null") -> Nothing
    (String v) -> return v
    Null -> Nothing
    _ -> Nothing


data Count = Count {
  lang :: T.Text,
  count :: Int
  } deriving Show


countLang ::  [T.Text] -> T.Text -> Count
countLang xs t = Count t (sum $ fmap (\x -> if x == t then 1 else 0) xs)


main :: IO ()
main = do
  let req = HTTP.parseRequest_ "https://api.github.com/users/acamino/repos"
      req' = HTTP.addRequestHeader hUserAgent "sebashack" req
  response <- HTTP.httpLBS req' 
  let body = HTTP.getResponseBody response
  case eitherDecode' body  of 
    Left error -> print error
    Right json -> do
      let (Just langs) = sequence $ filter (\elt -> elt /= Nothing) $ fmap (parseStringAttr "language") $ V.toList json
          langs' = (S.toList . S.fromList) langs
          count = fmap (countLang langs) langs'
      print count
