{-# LANGUAGE PatternGuards #-}

module HTTP.ChatServer ( chatServer, chatThread ) where

import HTTP.Response ( Response, jsPrintf, blank200, error404 )
import HTTP.LoginServer ( LoginMessage(..), Agent )

import TCP.Chan ( Input, Output, writeOutput, readInput )
import Data.Maybe ( fromMaybe )
import Control.Monad ( forM_ )

data Chat = Join Agent (String -> String)
          | Leave Agent
          | Say String

-- The javascript client can tell US how it wants us to respond, i.e.
-- where to store the answer...
chatServer :: Output Chat
           -> Agent -> [String] -> [(String,String)] -> IO Response
chatServer o a ps q = do putStrLn $ "chatServer <o> "++show a++" "++show ps++" "
                                  ++show q
                         chatServer' o a ps q

chatServer' :: Output Chat
           -> Agent -> [String] -> [(String,String)] -> IO Response
chatServer' outp a ["join"] q = do writeOutput outp $ Join a f
                                   writeOutput outp $ Say ("Welcome "++show a)
                                   blank200
    where f s = jsPrintf (fromMaybe "$.chat.say(%s+\"\\n\")" $ lookup "q" q) [s]
chatServer' outp a ["leave"] _ = do writeOutput outp $ Leave a
                                    writeOutput outp $ Say ("Goodbye "++show a)
                                    blank200
chatServer' outp a ["say"] q = do let msg = fromMaybe "(noinput)" $ lookup "q" q
                                  writeOutput outp $ Say (show a++": "++msg)
                                  blank200
chatServer' _ _ _ _ = putStrLn "chatServer _ _ _ _" >> error404

chatThread :: Input Chat -> Output LoginMessage
           -> [(Agent,String -> String)] -> IO ()
chatThread inp outp ags = do msg <- readInput inp
                             case msg of
                               Join a f -> chatThread inp outp $ replace a f ags
                               Leave a  -> chatThread inp outp $ remove a ags
                               Say s -> do putStrLn $"Say "++show s
                                           forM_ ags $ \(a,f) ->
                                               writeOutput outp $
                                               SendMessage a $ f s
                                           chatThread inp outp ags
    where replace a f [] = [(a,f)]
          replace a f ((a',f'):as) | a==a'     = (a,f):as
                                   | otherwise = (a',f'):replace a f as
          remove _ [] = []
          remove a ((a',f'):as) | a==a'     = as
                                | otherwise = (a',f'):remove a as
