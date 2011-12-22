--
-- Web redirector
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

import Prelude hiding (catch)

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe
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

import Lookup (lookupHash)

lookupTarget :: S.ByteString -> Snap S.ByteString
lookupTarget x = catch
    (liftIO $ lookupHash x)
    (\e -> do
        logError $ S.pack $ "Exception caught: " ++ show (e :: SomeException)
        return "")


serveJump :: Snap ()
serveJump = do
    h <-  getParam "hash"
    let h' = fromMaybe "" h
    t <- lookupTarget h'
    if t == ""
    then
        serveNotFound
    else
        redirect' t 301


serveError :: String -> Snap ()
serveError msg = do
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS "500 Internal Server Error\n"
    writeBS $ S.pack msg

--
-- If a key is requested that doesn't exist, we give 404. TODO
-- As this is probably a common case, we should serve a more
-- interesting page.
--

serveNotFound :: Snap ()
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    writeBS "404 Not Found"

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
     ("/:hash", serveJump)]
    <|> serveNotFound

main :: IO ()
main = quickHttpServe site

