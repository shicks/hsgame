{-# LANGUAGE PatternGuards #-}

module HTTP.Dominion ( dominionHandler, MessageToGame(..) ) where

import Dominion ( MessageToClient(Question, Info), QId, Answer,
                  ResponseFromClient(ResponseFromClient),
                  play, evalGame, pretty, pickDecks )
import qualified Dominion ( start )
import TCP.Chan ( Output, pipe, readInput, writeOutput )

import HTTP.Response ( Response, jsPrintf, blank200, error404 )
import HTTP.LoginServer ( Agent )
import HTTP.Handlers ( Handler(..), Message(..) )

import Data.Maybe ( fromMaybe )
import Control.Monad ( forM_, forever )
import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )

data MessageToGame = StartGame
                   | NewPlayer Agent (Output MessageToClient)

-- The javascript client can tell US how it wants us to respond, i.e.
-- where to store the answer...
dominionHandler :: String -> (Agent -> String -> IO ()) -> IO Handler
dominionHandler area sendmess =
    do (i,o) <- pipe -- ResponseFromClient
       latestq <- newEmptyMVar
       let handle a ps q =
               Message $ \st ->
                   do putStrLn $ "dominionHandler <o> "++show a++" "++
                               show ps++" " ++show q
                      handler area sendmess st a ps q
           startg ds = do decks <- pickDecks []
                          cls <- mapM (manageClient sendmess ds) (clients ds)
                          state <- Dominion.start cls i decks
                          forkIO (evalGame play state >>= print)
                          return ()
           initstate = DomState [] o latestq startg
       return $ Handler initstate handle

manageClient :: (Agent -> String -> IO ()) -> DomState -> (Agent, a)
             -> IO (String, Output MessageToClient)
manageClient sendmess ds (a,_) =
    do (i,o) <- pipe
       forkIO $ forever $
              do x <- readInput i
                 putStrLn ("got message for "++show a++":\n"++pretty x)
                 mapM_ (sayto sendmess ds a) (lines $ pretty x)
                 case x of
                   Question qid _ as mnmx ->
                       do putStrLn "got question..."
                          putMVar (latestQ ds) (qid, as, mnmx)
                   Info _ -> putStrLn "got info."
       return (show a, o)

data DomState = DomState { clients :: [(Agent, String -> String)],
                           game :: Output ResponseFromClient,
                           latestQ :: MVar (QId, [Answer], (Int,Int)),
                           start :: DomState -> IO () }

handler :: String -> (Agent -> String -> IO ())
        -> DomState
        -> Agent -> [String] -> [(String,String)]
        -> IO (DomState, Response)
handler _ sendmess ds _ ["start"] _ =
    do putStrLn "game should start now..."
       say sendmess ds "Game is starting!"
       say sendmess ds ("Players: "++unwords (map (show . fst) $ clients ds))
       start ds ds
       r <- blank200
       return (ds, r)
handler area sendmess ds a ["join"] q =
    do let clients' = (a,f) : filter ((/= a) . fst) (clients ds)
           ds' = ds { clients = clients' }
       say sendmess ds' ("Welcome "++show a)
       r <- blank200
       return (ds', r)
    where f s = jsPrintf (fromMaybe js $ lookup "q" q) [s]
          js = "$."++area++".say(%s)"
handler _ sendmess ds a ["leave"] _ =
    do let cl' = filter ((/= a) . fst) $ clients ds
           ds' = ds { clients = cl' }
       say sendmess ds' ("Goodbye "++show a)
       r <- blank200
       return (ds', r)
handler _ sendmess ags a ["say"] q =
    do let msg = fromMaybe "(noinput)" $ lookup "q" q
       say sendmess ags (show a++": "++msg)
       r <- blank200
       return (ags, r)
handler _ sendmess ds a ["answer"] q =
 do putStrLn "checking on the question... (should be maybe version)"
    (qid, as, (mn,mx)) <- takeMVar $ latestQ ds
    case lookup "q" q of
      Just ns ->
          do let toa x = case reads x of
                           [(num,"")] | num < 1 -> []
                                      | num > length as -> []
                                      | otherwise -> [as !! (num-1)]
                           _ -> []
                 myas = concatMap toa $ words ns
             if length myas > mx || length myas < mn
                then sayto sendmess ds a "Bad answer!"
                else writeOutput (game ds) $ ResponseFromClient qid myas
      _ -> putStrLn "This doesn't seem quite right..."
    r <- blank200
    return (ds, r)
handler _ _ ags _ _ _ =
    do putStrLn "handler _ _ _ _"
       r <- error404
       return (ags, r)

sayto :: (Agent -> String -> IO ()) -> DomState -> Agent -> String -> IO ()
sayto sendmess ags a s =
    case lookup a (clients ags) of
      Just f -> sendmess a (f s)
      Nothing -> return ()

say :: (Agent -> String -> IO ()) -> DomState -> String -> IO ()
say sendmess ags s =
    forM_ (clients ags) $ \(a,f) -> sendmess a (f s)
