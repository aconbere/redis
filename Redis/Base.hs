module Redis.Base where

import Network
import System.IO
import Data.List

data RedisValue = RedisString String
                | RedisInteger Int
                | RedisMulti [Maybe RedisValue] deriving (Show)

redis :: HostName -> PortNumber -> IO Handle
redis host port =
    withSocketsDo $ do
        h <- connectTo host (PortNumber port)
        hSetNewlineMode h (NewlineMode CRLF CRLF)
        hSetBuffering h NoBuffering
        return h


-- Comand helpers

command h f = do f >> getReply h

inline :: Handle -> String -> [String] -> IO()
inline h command args = hPutStrLn h $ concat $ intersperse " " ([command] ++ args)

bulk :: Handle -> String -> [String] -> String -> IO()
bulk handle command args value = do
    inline handle command (args ++ [(show $ length value)])
    hPutStrLn handle value

multiBulk :: Handle -> [[String]] -> IO()
multiBulk handle commands = do
    hPutStrLn handle ("*" ++ (show $ length commands))
    mapM_ (hPutStrLn handle) (concatMap formatBulkCommand commands)

multiBulk' :: Handle -> String -> [(String, String)] -> IO()
multiBulk' handle command kvs = do
    multiBulk handle (map (\(k, v) -> [command, k, v]) kvs)

formatBulkCommand :: [String] -> [String]
formatBulkCommand c =
    concat $ zipWith (\x y -> [x, y]) (map (\i -> "$" ++ (show $ length i)) c) c


-- Reply Helpers

getReply :: Handle -> IO (Maybe RedisValue)
getReply h = do
    prefix <- hGetChar h
    (getReplyType h prefix)

singleLineReply :: Handle -> IO (Maybe RedisValue)
singleLineReply h = do
    l <- hGetLine h
    return (Just (RedisString l))

integerReply :: Handle -> IO (Maybe RedisValue)
integerReply h = do
    l <- hGetLine h
    return (Just (RedisInteger (read l::Int)))

bulkReply :: Handle -> IO (Maybe RedisValue)
bulkReply h = do
    l <- hGetLine h
    let bytes = read l::Int

    if bytes == (-1)
        then return Nothing
        else do
            v <- takeChar bytes h
            hGetLine h -- cleans up
            return (Just (RedisString v))

multiBulkReply :: Handle -> IO (Maybe RedisValue)
multiBulkReply h = do
    l <- hGetLine h
    let items = read l::Int
    multiBulkReply' h items []

multiBulkReply' :: Handle -> Int -> [Maybe RedisValue] -> IO (Maybe RedisValue)
multiBulkReply' h 0 values = return (Just (RedisMulti values))
multiBulkReply' h n values = do
    hGetChar h -- discard the type data since we know it's a bulk string
    v <- bulkReply h
    multiBulkReply' h (n - 1) (values ++ [v])

getReplyType :: Handle -> Char -> IO (Maybe RedisValue)
getReplyType h prefix =
    case prefix of
        '$' -> bulkReply h
        ':' -> integerReply h
        '+' -> singleLineReply h
        '-' -> singleLineReply h
        '*' -> multiBulkReply h
        _ -> singleLineReply h

takeChar :: Int -> Handle -> IO(String)
takeChar n h = takeChar' n h ""

takeChar' :: Int -> Handle -> String -> IO(String)
takeChar' 0 h s = return s
takeChar' n h s = do
    char <- hGetChar h
    (takeChar' (n - 1) h (s ++ [char]))
