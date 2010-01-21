module Redis.Commands.List where

import Redis.Base

rightPush h key value = command h $ bulk h "RPUSH" [key] value
leftPush h key value = command h $ bulk h "LPUSH" [key] value
length h key = command h $ inline h "LLEN" [key]
range h key start end = command h $ inline h "LRANGE" [key, start, end]
trim h key start end = command h $ inline h "LTRIM" [key, start, end]
index h key index = command h $ inline h "LINDEX" [key, index]
set h key index value = command h $ bulk h "LSET" [key, index] value
remove h key count value = command h $ bulk h "LREM" [key, count] value
leftPop h key = command h $ inline h "LPOP" [key]
rightPop h key = command h $ inline h "RPOP" [key]
blockingLeftPop h keys timeout = command h $ inline h "BLPOP" (keys ++ timeout)
blockingRightPop h keys timeout = command h $ inline h "BRPOP" (keys ++ timeout)
blockingRightPopLeftPish h source dest = command h $ inline h "RPOPLPUSH" [source, dest]
