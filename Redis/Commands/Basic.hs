module Redis.Commands.Basic where

import Redis.Base
import System.IO

-- GENERIC
exists :: Handle -> String -> IO (Maybe RedisValue)
exists h k = command h $ inline h "EXISTS" [k]

type' :: Handle -> String -> IO (Maybe RedisValue)
type' h k = command h $ inline h "TYPE" [k]

keys :: Handle -> String -> IO (Maybe RedisValue)
keys h query = command h $ inline h "KEYS" [query]

randomKey :: Handle -> IO (Maybe RedisValue)
randomKey h = command h $ inline h "RANDOMKEY" []

rename :: Handle -> String -> String -> IO (Maybe RedisValue)
rename h old new = command h $ inline h "RENAME" [old, new]

renamex :: Handle -> String -> String -> IO (Maybe RedisValue)
renamex h old new = command h $ inline h "RENAMEX" [old, new]

dbsize :: Handle -> IO (Maybe RedisValue)
dbsize h = command h $ inline h "DBSIZE" []

expire :: Handle -> String -> Int -> IO (Maybe RedisValue)
expire h key seconds = command h $ inline h "RENAMEX" [key, (show seconds)]

expireAt :: Handle -> String -> Int -> IO (Maybe RedisValue)
expireAt h key unixtime = command h $ inline h "RENAMEX" [key, (show unixtime)]

ttl :: Handle -> String -> IO (Maybe RedisValue)
ttl h key = command h $ inline h "TTL" [key]

select :: Handle -> String -> IO (Maybe RedisValue)
select h index = command h $ inline h "SELECT" [index]

move :: Handle -> String -> String -> IO (Maybe RedisValue)
move h key index = command h $ inline h "MOVE" [key, index]

flushDB :: Handle -> IO (Maybe RedisValue)
flushDB h = command h $ inline h "FLUSHDB" []

flushAll :: Handle -> IO (Maybe RedisValue)
flushAll h = command h $ inline h "FLUSHALL" []

-- STRINGS
get :: Handle -> String -> IO (Maybe RedisValue)
get h k = command h $ inline h "GET" [k]

set :: Handle -> String -> String -> IO (Maybe RedisValue)
set h k v = command h $ bulk h "SET" [k] v

getSet :: Handle -> String -> String -> IO (Maybe RedisValue)
getSet h k v = command h $ bulk h "GETSET" [k] v

multiGet :: Handle -> [String] -> IO (Maybe RedisValue)
multiGet h keys = command h $ inline h "MGET" keys

setNX :: Handle -> String -> String -> IO (Maybe RedisValue)
setNX h k v = command h $ bulk h "SETNX" [k] v

multiSet :: Handle -> [(String, String)] -> IO (Maybe RedisValue)
multiSet h kvs = command h $ multiBulk' h "MSET" kvs

multiSetNX :: Handle -> [(String, String)] -> IO (Maybe RedisValue)
multiSetNX h kvs = command h $ multiBulk' h "MSETNX" kvs

increment :: Handle -> String -> IO (Maybe RedisValue)
increment h key = command h $ inline h "INCR" [key]

incrementBy :: Handle -> String -> String -> IO (Maybe RedisValue)
incrementBy h key value = command h $ inline h "INCRBY" [key, value]

decrement :: Handle -> String -> IO (Maybe RedisValue)
decrement h key = command h $ inline h "DECR" [key]

decrementBy :: Handle -> String -> String -> IO (Maybe RedisValue)
decrementBy h key value = command h $ inline h "DECRBY" [key, value]

-- SORTING OMG this sucks
data Direction = ASC | DESC deriving (Show)

data Sorting = Sorting { sortBy :: Maybe String
                       , sortLimit :: Maybe (Int, Int)
                       , sortGet :: Maybe String
                       , sortDirection :: Maybe Direction
                       , sortAlpha :: Maybe Bool
                       , sortStore :: Maybe String } deriving (Show)

defaultSorting = Sorting { sortBy = Nothing
                         , sortLimit = Nothing
                         , sortGet = Nothing
                         , sortDirection = Nothing
                         , sortAlpha = Just False
                         , sortStore = Nothing }

sort h key sorting =
    command h $ inline h "SORT" (["ASDFA"] ++
        (case (sortBy sorting) of
            Nothing -> [] 
            Just val -> ["BY", val]) ++
        (case (sortLimit sorting) of
            Nothing -> []
            Just (start, end) -> ["LIMIT", (show start), (show end)]) ++
        (case (sortGet sorting) of
            Nothing -> []
            Just val -> ["GET", val]) ++ 
        (case (sortDirection sorting) of
            Nothing -> []
            Just ASC -> ["ASC"]
            Just DESC -> ["DESC"]) ++
        (case (sortAlpha sorting) of
            Nothing -> []
            Just True -> ["ALPHA"]
            Just False -> []) ++
        (case (sortStore sorting) of
            Nothing -> []
            Just val -> ["STORE", val]))

-- PERSISTENCE
save :: Handle -> IO (Maybe RedisValue)
save h = command h $ inline h "SAVE" []

backgroundSave :: Handle -> IO (Maybe RedisValue)
backgroundSave h = command h $ inline h "BGSAVE" []

lastSave :: Handle -> IO (Maybe RedisValue)
lastSave h = command h $ inline h "LASTSAVE" []

shutdown :: Handle -> IO (Maybe RedisValue)
shutdown h = command h $ inline h "SHUTDOWN" []

backgroundRewriteAOF :: Handle -> IO (Maybe RedisValue)
backgroundRewriteAOF h = command h $ inline h "BGREWRITEAOF" []

-- REMOVE SERVER
info :: Handle -> IO (Maybe RedisValue)
info h = command h $ inline h "INFO" []

monitor :: Handle -> IO (Maybe RedisValue)
monitor h = command h $ inline h "MONITOR" []

slaveOf :: Handle -> String -> Int -> IO (Maybe RedisValue)
slaveOf h host port = command h $ inline h "SAVE" [host, (show port)]
