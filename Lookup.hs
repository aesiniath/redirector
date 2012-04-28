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

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Lookup (lookupHash, storeURL) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)
import Database.Redis
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (MonadCatchIO, bracket)
import Numeric (showHex)
import System.Random (randomRIO)

import Hashes (convert, encode, digest)

--
-- Process jump hash
--

fromReply :: (Either Reply (Maybe S.ByteString)) -> S.ByteString
fromReply x = 
    either first second x
  where
    first :: Reply -> S.ByteString
    first (Error s) = s
    first _         = ""

    second :: (Maybe S.ByteString) -> S.ByteString
    second = fromMaybe ""


queryTarget ::  S.ByteString -> Redis S.ByteString
queryTarget x = do
    k <- get key
    return $ fromReply k
  where
    key = S.append "target:" x


queryInverse :: S.ByteString -> Redis S.ByteString
queryInverse x = do
    k <- get key
    return $ fromReply k
  where
    key = S.append "inverse:" x



lookupHash :: S.ByteString -> IO S.ByteString
lookupHash x = bracket
    (connect defaultConnectInfo)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ queryTarget x)

--
-- Given a URL, generate a hash for it and store at that address. Return the
-- hash. Complications: first check to see that we haven't already stored that
-- URL; and, when storing, if the key already exists, we need to find choose
-- another.
--

findAvailableKey :: Redis S.ByteString
findAvailableKey = do
    num <- liftIO $ randomRIO (0, 62^5)
    let x  = encode num
        x' = S.pack x

    v <- queryTarget x'

    if S.null v
    then
        return x'
    else
        findAvailableKey


storeNewKey :: S.ByteString -> S.ByteString -> Redis S.ByteString
storeNewKey u' y' = do
    x' <- findAvailableKey
    let
        targetKey = S.append "target:" x'
        targetValue = u'
        inverseKey = S.append "inverse:" y'
        inverseValue = x'

    set targetKey targetValue
    set inverseKey inverseValue
    return x'


checkExistingKey :: S.ByteString -> Redis S.ByteString
checkExistingKey u' = do
    h' <- queryInverse y'

    if S.null h'
    then
        storeNewKey u' y'
    else
        return h'
  where
    y' = S.pack y
    y  = showHex s ""
    s  = digest u
    u  = S.unpack u'


storeURL :: S.ByteString -> IO S.ByteString
storeURL u' = bracket
    (connect defaultConnectInfo)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ checkExistingKey u')

