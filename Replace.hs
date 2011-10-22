{-# LANGUAGE OverloadedStrings #-}

import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import Control.Applicative
import Prelude hiding (length, appendFile, take)
import Data.ByteString.Char8
import Data.String
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.IO.Class

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


site :: Snap ()
site = route [ ("/time", serveTime) ]
    <|> serveDirectory "content/"


main :: IO ()
main = quickHttpServe site

