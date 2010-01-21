module Redis.Commands.Set where

import Redis.Base

add h key value = command h $ bulk h "SADD" [key] value
remove h key value = command h $ bulk h "SREM" [key] value
pop h key = command h $ inline h "SPOP" [key]
move h source dest = command h $ inline h "SMOVE" [source, dest]
cardinality h key = command h $ inline h "SCARD" [key]
isMember h key value = command h $ bulk h "SISMEMBER" [key] value
intersection h keys = command h $ inline h "SINTER" keys
storeIntersection h dstKey keys = command h $ inline h "SINTERSTORE" ([dstKey] ++ keys)
union h keys = command h $ inline h "SUNION" keys
storeUnion h dstKey keys = command h $ inline h "SUNIONSTORE" ([dstKey] ++ keys)
diff h keys = command h $ inline h "SDIFF" keys
storeDiff h dstKey keys = command h $ inline h "SDIFFSTORE" ([dstKey] ++ keys)
members h key = command h $ inline h "SMEMBERS" [key]
randomMember h key = command h $ inline h "SRANDMEMBER" [key]
