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

import Hashes (convert)

--
-- Process jump hash
--

fromValue :: RedisValue -> S.ByteString
fromValue v = case v of
        RedisString s   -> s
        RedisInteger i  -> S.pack $ show i
        RedisNil        -> "" -- used to be "(nil)"
        RedisMulti vs   -> S.intercalate "\n" $ map fromValue vs



queryKey :: Server -> S.ByteString -> IO S.ByteString
queryKey r x = do
        k <- get r key
        return $ fromValue k
    where
        key = toParam $ S.append "target:" x


lookupHash :: S.ByteString -> IO S.ByteString
lookupHash x = bracket
        (connect "localhost" 6379)
        (disconnect)
        (\r -> queryKey r x)

--
-- Given a URL, calculate a hash for it and store at that address.
-- Return the hash
--


storeKey :: Server -> S.ByteString -> IO S.ByteString
storeKey r v = do
        result <- set r key value
        if result 
        then
            return x
        else
            return "Problem!"
    where
        key = toParam $ S.append "target:" x
        value = toParam v
        x = S.pack $ convert url
        url = S.unpack v


storeURL :: S.ByteString -> IO S.ByteString
storeURL u = bracket
        (connect "localhost" 6379)
        (disconnect)
        (\r -> storeKey r u)

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

