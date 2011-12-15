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

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe
import Control.Applicative
import Prelude hiding (length, appendFile, take, concat, foldr, head)
import qualified Data.ByteString.Char8 as Strict
import Data.Maybe (fromMaybe)
--
-- Process jump hash
--

serveJump :: Snap ()
serveJump = do
    switch <- getParam "switch"
    hash   <- getParam "hash"
    writeBS $ Strict.concat [ fromMaybe "Nothing" switch, "\n", fromMaybe "Nothing" hash ]  

serveNotFound :: Snap ()
serveNotFound = undefined

--
-- Top level URL routing logic.
--

site :: Snap ()
site = route
    [("/:switch/:hash", serveJump)]
    <|> serveNotFound

main :: IO ()
main = quickHttpServe site

