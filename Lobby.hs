module Lobby ( lobby, lobbyClient ) where

import TCP.Client ( Client, ioClient, forkClient )
import TCP.ServerTypes ( Server, ServerMessage(N,M),
                         ioServer, forkServer )
import TCP.Chan ( ShowRead, writeOutput, readInput, pipe, readLine )
import TCP.Message ( Message(..) )

import Data.Maybe ( fromJust )
import Control.Monad ( forever )
import Control.Concurrent ( forkIO )

data LobbyMsg tablename tablemsg = Chat String
                                 | JoinTable tablename
                                 | TableMsg tablename tablemsg
                                   deriving ( Eq, Show, Read )
instance (ShowRead name, ShowRead msg) => ShowRead (LobbyMsg name msg)

data LobbyClient name tablename tablemsg
    = Chatted name String
    | JoinedTable name tablename
    | AtTable tablename tablemsg
    | Joined name
      deriving ( Eq, Show, Read )
instance (ShowRead name, ShowRead tablename, ShowRead msg) =>
         ShowRead (LobbyClient name tablename msg)

lobby :: (Eq name, ShowRead name,
          Eq tablename, ShowRead tablename,
          ShowRead toclient, ShowRead toserver) =>
         tablename -> -- hokey trick to define tablename type...
         Server name toclient (ServerMessage toserver)
      -> Server name (LobbyClient name tablename toclient)
                     (ServerMessage (LobbyMsg tablename toserver))
lobby _ mktable = ioServer $ \o i ->
    do let handle us ts =
               do let sendToOthers m =
                          mapM_ (\t ->
                                 if t /= fromAgent m
                                 then writeOutput o (m { toAgent = t })
                                 else return ()) us
                      sendToAll m =
                          mapM_ (\t -> writeOutput o (m { fromAgent= toAgent m,
                                                          toAgent = t })) us
                  m <- readInput i
                  case m of
                    Message f t N ->
                        do putStrLn (show f++" joined!")
                           -- the following message is probably ignored...
                           writeOutput o (Message f f (Joined f))
                           sendToAll (Message f t (Joined f))
                           handle (f:filter (/= f) us) ts
                    Message f t (M (Chat c)) ->
                        do putStrLn (show f++" says "++c)
                           sendToOthers (Message f t (Chatted f c))
                           handle us ts
                    Message f t (M (JoinTable tab)) ->
                        case lookup tab ts of
                          Just table ->
                              do writeOutput table (Message f t N)
                                 writeOutput o (Message f f (JoinedTable f tab))
                                 sendToAll (Message f t (JoinedTable f tab))
                                 handle us ts
                          Nothing ->
                              do putStrLn ("creating table "++show tab)
                                 (ii,oo) <- forkServer mktable
                                 -- Any messages emerging from the
                                 -- table get wrapped up with a table
                                 -- label and forwarded to the
                                 -- clients.
                                 forkIO $ forever $
                                        do Message f' t' x <- readInput ii
                                           writeOutput o
                                               (Message f' t' (AtTable tab x))
                                 writeOutput oo (Message f t N)
                                 writeOutput o (Message f f (JoinedTable f tab))
                                 sendToAll (Message f t (JoinedTable f tab))
                                 handle us ((tab,oo):ts)
                    Message f t (M (TableMsg tab mm)) ->
                        case lookup tab ts of
                          Just table ->
                              do putStrLn $ "passing message "++show mm++
                                            " to table "++ show tab
                                 writeOutput table (Message f t (M mm))
                                 handle us ts
                          Nothing ->
                              do putStrLn "BOGUS MESSAGE!!!"
                                 handle us ts
       handle [] []
                         
lobbyClient :: (Eq tablename, ShowRead tablename,
                ShowRead toclient, ShowRead toserver) =>
               tablename -> Client toclient toserver
            -> Client (LobbyClient String tablename toclient)
                      (LobbyMsg tablename toserver)
lobbyClient _ dotable = ioClient $ \i o ->
    do -- Don't grab stdin until *after* we've gotten one input over
       -- the chan. This gives any modifiers a chance to get some
       -- input first.
       --
       -- I'm telling it to ignore the first message, so it had better
       -- be an awknowledgement message.
       Joined me <- readInput i
       putStrLn ("My name is "++show me)
       (tablei, tableo) <- pipe
       let dumpout mytab@(Just (mytable, tableout)) =
               do m <- readInput i
                  case m of
                    Chatted _ _ -> dumpout mytab
                    JoinedTable _ _ -> dumpout mytab
                    Joined _ -> dumpout mytab
                    AtTable t x | t == mytable ->
                                    do writeOutput tableout x
                                       dumpout mytab
                                | otherwise -> dumpout mytab
           dumpout Nothing =
               do m <- readInput i
                  case m of
                    Chatted name mm ->
                        do putStrLn (show name++" says "++mm)
                           dumpout Nothing
                    JoinedTable n t
                        | n == me ->
                            do putStrLn ("I joined table "++show t)
                               tchan <- readInput tablei
                               dumpout (Just (t, tchan))
                        | otherwise ->
                            do putStrLn (show n++" joined "++show t)
                               dumpout Nothing
                    Joined n ->
                        do putStrLn (show n++" has joined us.")
                           dumpout Nothing
                    AtTable _ _ -> dumpout Nothing
       forkIO $ dumpout Nothing
       forever $ do x <- getLine
                    if take 5 x == "join "
                       then do let tname = fromJust $ readLine (drop 5 x)
                               writeOutput o (JoinTable tname)
                               (ii,oo) <- forkClient dotable
                               -- send the output chan to the other thread
                               writeOutput tableo oo
                               forever $ do m <- readInput ii
                                            writeOutput o (TableMsg tname m)
                       else writeOutput o (Chat x)
