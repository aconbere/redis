module Redis.Commands.SortedSet where

import Redis.Base

add key score member = "ZADD"
remove key member = "ZREM"
incrementBy key = "ZINCRBY"
range key start end = "ZRANGE"
reverseRange key start end = "ZREVRANGE"
rangeByScore key min max = "ZRANGEBYSCORE"
cardinality key = "ZCARD"
score key element = "ZSCORE"
removeRangeByScore key min max = "ZREMRANGEBYSCORE"


