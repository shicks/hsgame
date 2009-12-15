{-# LANGUAGE PatternGuards #-}

module HTTP.ChatHandler ( chatHandler ) where

import HTTP.Response ( Response, jsPrintf, blank200, error404 )
import HTTP.LoginServer ( Agent )
import HTTP.Handlers ( Handler(..), Message(..) )

import Data.Maybe ( fromMaybe )
import Control.Monad ( forM_ )

-- The javascript client can tell US how it wants us to respond, i.e.
-- where to store the answer...
chatHandler :: String -> (Agent -> String -> IO ()) -> IO Handler
chatHandler area sendmess = return $ Handler [] handler
    where handler a ps q =
            Message $ \st ->
            do putStrLn $ "chatHandler <o> "++show a++" "++show ps++" " ++show q
               chatHandler' area sendmess st a ps q

chatHandler' :: String -> (Agent -> String -> IO ())
             -> [(Agent,String -> String)]
             -> Agent -> [String] -> [(String,String)]
             -> IO ([(Agent,String -> String)], Response)
chatHandler' area sendmess ags a ["join"] q =
    do let ags' = (a,f) : filter ((/= a) . fst) ags
       say sendmess ags' ("Welcome "++show a)
       r <- blank200
       return (ags', r)
    where f s = jsPrintf (fromMaybe js $ lookup "q" q) [s]
          js = "$."++area++".say(%s+\"\\n\")"
chatHandler' _ sendmess ags a ["leave"] _ =
    do let ags' = filter ((/= a) . fst) ags
       say sendmess ags' ("Goodbye "++show a)
       r <- blank200
       return (ags', r)
chatHandler' _ sendmess ags a ["say"] q =
    do let msg = fromMaybe "(noinput)" $ lookup "q" q
       say sendmess ags (show a++": "++msg)
       r <- blank200
       return (ags, r)
chatHandler' _ _ ags _ _ _ =
    do putStrLn "chatHandler _ _ _ _"
       r <- error404
       return (ags, r)

say :: (Agent -> String -> IO ()) -> [(Agent,String -> String)]
    -> String -> IO ()
say sendmess ags s =
    forM_ ags $ \(a,f) -> sendmess a (f s)

