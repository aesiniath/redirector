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
import Database.Redis hiding (toParam)
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (MonadCatchIO, bracket)
import Numeric (showHex)
import System.Random (randomRIO)

import Hashes (convert, encode, digest)

--
-- Process jump hash
--

fromValue :: RedisValue -> S.ByteString
fromValue v = case v of
        RedisString s   -> s
        RedisInteger i  -> S.pack $ show i
        RedisNil        -> "" -- used to be "(nil)"
        RedisMulti vs   -> S.intercalate "\n" $ map fromValue vs



queryTarget ::  Server -> S.ByteString -> IO S.ByteString
queryTarget r x = do
        k <- get r key
        return $ fromValue k
    where
        key = toParam $ S.append "target:" x


queryInverse :: Server -> String -> IO S.ByteString
queryInverse r x = do
        k <- get r key
        return $ fromValue k
    where
        key = toParam $ "inverse:" ++ x



lookupHash :: S.ByteString -> IO S.ByteString
lookupHash x = bracket
        (connect "localhost" 6379)
        (disconnect)
        (\r -> queryTarget r x)

--
-- Given a URL, generate a hash for it and store at that address. Return the
-- hash. Complications: first check to see that we haven't already stored that
-- URL; and, when storing, if the key already exists, we need to find choose
-- another.
--

findAvailableKey :: Server -> IO String
findAvailableKey r = do
        num <- randomRIO (0, 62^5)
        let x = encode num
        let x' = S.pack x

        v <- queryTarget r x'

        if S.null v
        then
            return x
        else
            findAvailableKey r


storeNewKey :: Server -> S.ByteString -> String -> IO S.ByteString
storeNewKey r u y = do
        x <- findAvailableKey r
        let
            targetKey = toParam $ "target:" ++ x
            targetValue = toParam u
            inverseKey = toParam $ "inverse:" ++ y
            inverseValue = toParam x

        set r targetKey targetValue
        set r inverseKey inverseValue
        return $ S.pack x


checkExistingKey :: Server -> S.ByteString -> IO S.ByteString
checkExistingKey r u = do
        h <- queryInverse r y

        if S.null h
        then
            storeNewKey r u y
        else
            return h
    where
        y = showHex s ""
        s = digest $ S.unpack u


storeURL :: S.ByteString -> IO S.ByteString
storeURL u = bracket
        (connect "localhost" 6379)
        (disconnect)
        (\r -> checkExistingKey r u)

--
-- redis-haskell requires Lazy ByteStrings as parameters. Unfortunately, the
-- toParam function that ships with that library uses show, which messes up
-- Strings by encoding them.  
--

class Convert a where
    view :: a -> L.ByteString

instance Convert L.ByteString where
    view s = s

instance Convert S.ByteString where
    view s = L.fromChunks [s]

instance Convert Char where
    view c = L.singleton c

instance Convert [Char] where
    view cs = L.pack cs

instance Convert Int where
    view x = L.pack $ show x


toParam :: (Convert a) => a -> L.ByteString
toParam x = view x

