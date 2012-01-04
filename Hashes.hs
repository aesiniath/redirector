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

module Hashes (encode, decode) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)
import Numeric (showIntAtBase)
import Data.Char (isDigit, isUpper, isLower, chr, ord)

--
-- Conversion between decimal and base 62
--

represent :: Int -> Char
represent x | x < 10 = chr (48 + x)
            | x < 36 = chr (65 + x - 10)
            | x < 62 = chr (97 + x - 36)
            | otherwise = '@'

encode :: Int -> String
encode x    = showIntAtBase 62 represent x ""


value :: Char -> Int
value c     | isDigit c = ord c - 48
            | isUpper c = ord c - 65 + 10
            | isLower c = ord c - 97 + 36
            | otherwise = 0

multiply :: Int -> Char -> Int
multiply acc c = acc * 62 + value c

decode :: String -> Int
decode ss   = foldl multiply 0 ss

calculateHash :: String -> Int
calculateHash ss = undefined
