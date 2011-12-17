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
import qualified Data.ByteString.Char8 as Strict
import Data.Maybe (fromMaybe)
import Numeric
import Data.Char

--
-- Conversion between decimal and base 62
--

represent :: Int -> Char
represent x | x < 10 = chr (48 + x)
            | x < 36 = chr (65 + x - 10)
            | x < 62 = chr (97 + x - 36)
            | otherwise = '@'

encode :: Int -> String
encode x   = showIntAtBase 62 represent x ""


multiply :: Int -> Char -> Int
multiply acc c = acc * 62 + value c
    where
        value c | isDigit c = ord c - 48
        value c | isUpper c = ord c - 65 + 10
        value c | isLower c = ord c - 97 + 36
        value c | otherwise = 0

decode :: String -> Int
decode ss   = foldl multiply 0 ss

--
-- Process jump hash
--

lookupHash :: String -> String
lookupHash = undefined

serveJump :: Snap ()
serveJump = do
    switch <- getParam "switch"
    hash   <- getParam "hash"
    redirect' "http://www.operationaldynamics.com/" 301

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

