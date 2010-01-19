module Redis.Base where

import Network
import Network.Socket
import System.IO

data RedisValue = RedisString String | RedisInteger Int deriving (Show)

redis :: HostName -> PortNumber -> IO Handle
redis host port =
    withSocketsDo $ do
        h <- connectTo host (PortNumber port)
        hSetNewlineMode h (NewlineMode CRLF CRLF)
        hSetBuffering h NoBuffering
        return h

inline_query :: Handle -> String -> String -> IO()
inline_query h command value = hPutStrLn h (command ++ " " ++ value)

inline :: Handle -> String -> IO()
inline = hPutStrLn

bulk :: Handle -> String -> String -> String -> IO()
bulk handle command key value = do
    hPutStrLn handle (command ++ " " ++ key ++ " " ++  " " ++ show (length value))
    hPutStrLn handle value

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

--bulkReply :: Handle -> IO (Maybe RedisValue)
bulkReply h = do
    l <- hGetLine h
    let bytes = read l::Int

    if bytes == (-1)
        then return Nothing
        else do
            v <- takeChar bytes h
            return (Just (RedisString v))

getReplyType :: Handle -> Char -> IO (Maybe RedisValue)
getReplyType h prefix =
    case prefix of
        '$' -> bulkReply h
        ':' -> integerReply h
        '+' -> singleLineReply h

takeChar n h = takeChar' n h ""

takeChar' :: Int -> Handle -> String -> IO(String)
takeChar' 0 h s = return s
takeChar' n h s = do
    char <- hGetChar h
    (takeChar' (n - 1) h (s ++ [char]))
