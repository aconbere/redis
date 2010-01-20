module Redis.Commands where

import Redis.Base

command h f = do f >> getReply h


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
flushDB h = command h $ inline h "FLUSHDB"
flushAll h = command h $ inline h "FLUSHALL"

-- STRINGS
get h k = command h $ inline h "GET" [k]
set h k v = command h $ bulk h "SET" k v
