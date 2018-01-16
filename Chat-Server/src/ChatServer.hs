{-# LANGUAGE OverloadedStrings #-}
module ChatServer where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection) --username, connection

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client clients = filter ((/= fst client) . fst) clients

clientExists :: Client -> ServerState -> Bool
clientExists client clients = any ((== fst client) . fst) clients

sendMessageWithoutRage :: Text -> Text -> MVar ServerState -> IO()
sendMessageWithoutRage user msg state = 
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` T.replace "hate" "love" msg)

sendMessageWithForceLovingCats :: Text -> Text -> MVar ServerState -> IO()
sendMessageWithForceLovingCats user msg state =
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` T.replace "love dog" "love cat" msg)

checkIfContainsRage :: Text -> Bool
checkIfContainsRage msg = T.isInfixOf "hate" msg

checkIfNotLovingCats :: Text -> Bool
checkIfNotLovingCats msg = T.isInfixOf "love dog" msg

broadcast :: Text -> ServerState -> IO()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

server :: IO()
server = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
        _   | any ($ fst client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData conn (msg `mappend` "Name connot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)

            | clientExists client clients ->
                WS.sendTextData conn ("User already exists" :: Text)
            
            | otherwise -> flip finally disconnect $ do
                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData conn $
                        "Welcome! Users: " `mappend`
                        T.intercalate ", " (map fst s)
                    broadcast (fst client `mappend` " joined") s'
                    return s'
                talk conn state client
            where
                prefix  = "LOGIN:"
                client  = (T.drop (T.length prefix) msg, conn)
                disconnect = do
                    s <- modifyMVar state $ \s ->
                        let s' = removeClient client s in return (s', s')
                    broadcast (fst client `mappend` " disconnected") s

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- WS.receiveData conn
    case msg of
        _   | checkIfContainsRage msg -> sendMessageWithoutRage user msg state
            | checkIfNotLovingCats msg ->sendMessageWithForceLovingCats user msg state
            | otherwise -> readMVar state >>= broadcast
                (user `mappend` ": " `mappend` msg)
