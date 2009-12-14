{-# LANGUAGE PatternGuards #-}

module HTTP.ChatServer ( chatServer, chatThread, chatHandler ) where

import HTTP.Response ( Response, jsPrintf, blank200, error404 )
import HTTP.LoginServer ( LoginMessage(..), Agent )
import HTTP.Handlers ( Handler(..), Message(..) )

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
chatThread inp outp ags =
    do msg <- readInput inp
       case msg of
         Join a f -> chatThread inp outp $ (a,f) : filter ((/= a) . fst) ags
         Leave a  -> chatThread inp outp $ filter ((/= a) . fst) ags
         Say s -> do putStrLn $"Say "++show s
                     forM_ ags $ \(a,f) ->
                         writeOutput outp $ SendMessage a $ f s
                     chatThread inp outp ags


-- The javascript client can tell US how it wants us to respond, i.e.
-- where to store the answer...
chatHandler :: (Agent -> String -> IO ()) -> IO Handler
chatHandler sendmess = return $ Handler [] handler
    where handler a ps q =
            Message $ \st ->
            do putStrLn $ "chatHandler <o> "++show a++" "++show ps++" " ++show q
               chatHandler' sendmess st a ps q

chatHandler' :: (Agent -> String -> IO ())
             -> [(Agent,String -> String)]
             -> Agent -> [String] -> [(String,String)]
             -> IO ([(Agent,String -> String)], Response)
chatHandler' sendmess ags a ["join"] q =
    do let ags' = (a,f) : filter ((/= a) . fst) ags
       say sendmess ags' ("Welcome "++show a)
       r <- blank200
       return (ags', r)
    where f s = jsPrintf (fromMaybe "$.chat.say(%s+\"\\n\")" $ lookup "q" q) [s]
chatHandler' sendmess ags a ["leave"] _ =
    do let ags' = filter ((/= a) . fst) ags
       say sendmess ags' ("Goodbye "++show a)
       r <- blank200
       return (ags', r)
chatHandler' sendmess ags a ["say"] q =
    do let msg = fromMaybe "(noinput)" $ lookup "q" q
       say sendmess ags (show a++": "++msg)
       r <- blank200
       return (ags, r)
chatHandler' _ ags _ _ _ =
    do putStrLn "chatHandler _ _ _ _"
       r <- error404
       return (ags, r)

say :: (Agent -> String -> IO ()) -> [(Agent,String -> String)]
    -> String -> IO ()
say sendmess ags s =
    forM_ ags $ \(a,f) -> sendmess a (f s)

