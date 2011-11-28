--
-- Experiments with the Snap web programming framework
--
-- Copyright © 2011 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made available
-- to you by its authors as open source software: you can redistribute it
-- and/or modify it under the terms of the GNU General Public License version
-- 2 ("GPL") as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GPL for more details.
--
-- You should have received a copy of the GPL along with this program. If not,
-- see http://www.gnu.org/licenses/. The authors of this program may be
-- contacted through http://research.operationaldynamics.com/
--

{-# LANGUAGE OverloadedStrings #-}

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe
import Control.Applicative
import Prelude hiding (length, appendFile, take, concat, foldr, head)
import Data.ByteString.Char8 hiding (foldr, head)
import Data.String (fromString)
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.Trans (liftIO)
import Data.Map (Map, foldWithKey)
import Data.CaseInsensitive (CI, original)
import Data.List (foldr, head)
import Data.Map (foldrWithKey)

--
-- Utility function to output a timestamp in our standard format, with 
-- milisecond precision. You certainly don't need that across the net, but it
-- makes for a useful signature in logs when things are busy.
--

formatTimestamp :: UTCTime -> String
formatTimestamp x = formatTime defaultTimeLocale "%a %e %b %y, %H:%M:%S.%q" x


getTimestamp :: IO ByteString
getTimestamp = do
    cur <- getCurrentTime
    let time = fromString $ formatTimestamp cur
    let len  = length "Sat  8 Oct 11, 07:12:21.999"
    let str = take len time
    return $ append str "Z\n"


--
-- Serve such a timestamp. text/plain is of course the default MIME type but
-- this shows how to set it explicitly; presumably you'd need to do that for
-- most handlers in normal use.
--

serveTime :: Snap ()
serveTime = do
    time <- liftIO getTimestamp
    writeBS $ time
    modifyResponse $ setContentType "text/plain"


--
-- Serve the browser's HTTP headers back to them.
--

combineHeaders :: (CI ByteString, ByteString) -> ByteString -> ByteString
combineHeaders (k,v) acc = append acc $ concat [key, ": ", value, "\n"]
    where
        key = original k
        value = v


join :: Headers -> ByteString
join m = foldr combineHeaders "" $ listHeaders m


serveHeaders :: Snap ()
serveHeaders = do
    req <- getRequest
    let h = headers req
    writeBS $ join h


--
-- Explore handling query string parameters. Params is not an opaque type like
-- Headers is, so requires slightly different treatment.
--

combineParams :: ByteString -> [ByteString] -> ByteString -> ByteString
combineParams k v acc = append acc $ concat [key, ": ", values, "\n"]
    where
        key = k
        values = intercalate "; " v

    
serveRequest :: Snap ()
serveRequest = do
    p <- getParams
    writeBS $ foldrWithKey combineParams "" p


--
-- Top level URL routing logic.
--

site :: Snap ()
site = route
    [("/time", serveTime),
     ("/request", serveRequest),
     ("/headers", serveHeaders)]
    <|> serveDirectory "content/"


main :: IO ()
main = quickHttpServe site

