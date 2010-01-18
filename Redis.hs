module Redis () where

import Network
import Network.Socket
import System.IO

redis host port =
    withSocketsDo $ do
        h <- connectTo host (PortNumber port)
        hSetNewlineMode h (NewlineMode CRLF CRLF)
        hSetBuffering h NoBuffering
        return h

inline_query h command value = hPutStrLn h (command ++ " " ++ value)
inline = hPutStrLn

bulk :: Handle -> String -> String -> String -> IO()
bulk handle command key value = do
    hPutStrLn handle (command ++ " " ++ key ++ " " ++  " " ++ show (length value))
    hPutStrLn handle value

reply :: Handle -> IO (Maybe String)
reply h = do
    prefix <- hGetChar h
    (doReply h prefix)

singleLineReply :: Handle -> IO (Maybe String)
singleLineReply h = do
    l <- hGetLine h
    return (Just l)

integerReply :: Handle -> IO (Maybe String)
integerReply h = do
    l <- hGetLine h
    return (Just l)

bulkReply :: Handle -> IO (Maybe String)
bulkReply h = do
    l <- hGetLine h
    let lint = read l::Int

    if lint == (-1)
        then return Nothing
        else do
            l <- hGetLine h
            return (Just l)

doReply :: Handle -> Char -> IO (Maybe String)
doReply h prefix =
    case prefix of
        '$' -> bulkReply h
        ':' -> integerReply h
        '+' -> singleLineReply h
