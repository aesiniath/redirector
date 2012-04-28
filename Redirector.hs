--
-- Web redirector
--
-- Copyright © 2011-2012 Operational Dynamics Consulting, Pty Ltd
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

import Prelude hiding (catch)

import Snap.Http.Server
import Snap.Core
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)
import Numeric
import Data.Char
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (catch, throw)
import Control.Exception (SomeException)

import Lookup (lookupHash, storeURL)

lookupTarget :: S.ByteString -> Snap S.ByteString
lookupTarget x' = catch
    (liftIO $ lookupHash x')
    (\e -> do
        serveError x' e
        return "")


serveJump :: Snap ()
serveJump = do
    h  <- getParam "hash"
    t' <- lookupTarget $ fromMaybe "" h
    if t' == ""
    then
        serveNotFound
    else
        redirect' t' 301


serveError :: S.ByteString -> SomeException -> Snap ()
serveError x' e = do
    logError msg
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS "500 Internal Server Error\n"
    r <- getResponse
    finishWith r
  where
    msg = S.concat ["Looking up \"", x' , "\", ", S.pack $ show (e :: SomeException)]


--
-- If a key is requested that doesn't exist, we give 404.
--

serveNotFound :: Snap ()
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    sendFile "content/404.html"

--
-- Allow people to add URLs
--

serveBadRequest :: Snap ()
serveBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "400 Bad Request\n"


storeTarget :: S.ByteString -> Snap S.ByteString
storeTarget x' = catch
    (liftIO $ storeURL x')
    (\e -> do
        serveError x' e
        return "")


serveAdd :: Snap ()
serveAdd = do
    q <- getParam "url"
    case q of
        Just u'  -> do
            x' <- storeTarget u'
            writeBS "http://odyn.co/"
            writeBS x'
            writeBS "\n"
        Nothing -> serveBadRequest

--
-- If they request / then we send them to the corporate home page 
--

serveHome :: Snap ()
serveHome = do
    redirect' "http://www.operationaldynamics.com/" 302


--
-- Top level URL routing logic.
--

site :: Snap ()
site = route
    [("/", serveHome),
     ("/add", method POST serveAdd),
     ("/:hash", serveJump)]
    <|> serveNotFound

main :: IO ()
main = quickHttpServe site

