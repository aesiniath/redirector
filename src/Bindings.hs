{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (length, putStrLn, concat)
import Data.ByteString.Char8 hiding (map)
import Database.Redis
import System.Posix.Process (getProcessID)
import Data.Serialize (encode)
import Data.String (fromString)
import System.Random

genesis :: IO ()
genesis = do
    r <- connect "localhost" 6379
    p <- getProcessID
    n <- randomRIO (100,999)
    let i = pack $ show p ++ "+" ++ show (n :: Int)
    rpush r "items" $ toParam i
    return ()


exodus :: IO ()
exodus = do
    r <- connect "localhost" 6379
    x <- lrange r "items" 0 (-1)
    putStrLn $ fromValue x
    disconnect r

fromValue :: RedisValue -> ByteString
fromValue v = case v of
        RedisString s   -> s
        RedisInteger i  -> pack $ show i
        RedisNil        -> "(nil)"
        RedisMulti vs   -> intercalate "\n" $ map fromValue vs

mock = RedisMulti
        [ RedisInteger 65
        , RedisString "str"
        , RedisNil
        , RedisString "more"
        , RedisInteger 3 ]

test = putStrLn $ fromValue mock


leviticus :: IO ()
leviticus = do
    r <- connect "localhost" 6379
    x <- lpop r "items"
    putStrLn $ fromValue x
    disconnect r

main :: IO ()
main = do
    genesis
    exodus
    leviticus

