module Redis.Commands.Basic where

import Redis.Base

-- GENERIC
exists h k = command h $ inline h "EXISTS" [k]
type' h k = command h $ inline h "TYPE" [k]
keys h query = command h $ inline h "KEYS" [query]
randomKey h = command h $ inline h "RANDOMKEY" []
rename h old new = command h $ inline h "RENAME" [old, new]
renamex h old new = command h $ inline h "RENAMEX" [old, new]
dbsize h = command h $ inline h "DBSIZE" []
expire h key seconds = command h $ inline h "RENAMEX" [key, seconds]
expireAt h key unixtime = command h $ inline h "RENAMEX" [key, unixtime]
ttl h key = command h $ inline h "TTL" [key]
select h index = command h $ inline h "SELECT" [index]
move h key index = command h $ inline h "MOVE" [key, index]
flushDB h = command h $ inline h "FLUSHDB" []
flushAll h = command h $ inline h "FLUSHALL" []

-- STRINGS
get h k = command h $ inline h "GET" [k]
set h k v = command h $ bulk h "SET" [k] v
getSet h k v = command h $ bulk h "GETSET" [k] v
multiGet h keys = command h $ inline h "MGET" keys
setNX h k v = command h $ bulk h "SETNX" [k] v
multiSet h kvs = command h $ multiBulk' h "MSET" kvs
multiSetNX h kvs = command h $ multiBulk' h "MSETNX" kvs
increment h key = command h $ inline h "INCR" [key]
incrementBy h key value = command h $ inline h "INCRBY" [key, value]
decrement h key = command h $ inline h "DECR" [key]
decrementBy h key value = command h $ inline h "DECRBY" [key, value]

-- SORTING
-- sort <- oh crap!

-- PERSISTENCE
save h = command h $ inline h "SAVE" []
backgroundSave h = command h $ inline h "BGSAVE" []
lastSave h = command h $ inline h "LASTSAVE" []
shutdown h = command h $ inline h "SHUTDOWN" []
backgroundRewriteAOF h = command h $ inline h "BGREWRITEAOF" []

-- REMOVE SERVER
info h = command h $ inline h "INFO" []
monitor h = command h $ inline h "MONITOR" []
slaveOf h host port = command h $ inline h "SAVE" [host, port]
