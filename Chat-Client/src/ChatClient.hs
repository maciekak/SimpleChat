{-# LANGUAGE OverloadedStrings #-}
module ChatClient where

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.IO.Class
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    putStrLn "What's your name?"

    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg
        
    name <- T.getLine
    WS.sendTextData conn ("LOGIN:" `mappend` name)   
    
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop
    
    loop
    WS.sendClose conn ("Bye!" :: Text)

client :: IO()
client = WS.runClient "127.0.0.1" 9160 "/" app

