{-# LANGUAGE OverloadedStrings #-}

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe
import Control.Applicative
import Prelude hiding (length, appendFile, take, concat, foldr)
import Data.ByteString.Char8 hiding (foldr)
import Data.String (fromString)
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map, foldWithKey)
import Data.CaseInsensitive (CI, original)
import Data.List (foldr)


formatTimestamp :: UTCTime -> String
formatTimestamp x = formatTime defaultTimeLocale "%a, %e %b %y %H:%M:%S.%q" x

getTimestamp :: IO ByteString
getTimestamp = do
    cur <- getCurrentTime
    let time = fromString $ formatTimestamp cur
    let len = length "Sat,  8 Oct 11 07:12:21.7"
    return $ take len time


serveTime :: Snap ()
serveTime = do
    time <- liftIO getTimestamp
    writeBS $ append time "Z\n"


combine :: (CI ByteString, ByteString) -> ByteString -> ByteString
combine (k,v) acc = append acc $ concat [key, ": ", value, "\n"]
    where
        key = original k
        value = v


join :: Headers -> ByteString
join m = foldr combine "" $ listHeaders m


serveHeaders :: Snap ()
serveHeaders = do
    req <- getRequest
    let h = headers req
    writeBS $ join h


site :: Snap ()
site = route
    [("/time", serveTime),
     ("/headers", serveHeaders)]
    <|> serveDirectory "content/"


main :: IO ()
main = quickHttpServe site

