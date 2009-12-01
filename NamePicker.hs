module NamePicker ( pickNames, namedClient, simpleNamedClient ) where

import TCP.Client ( Client, ioClient, forkClient )
import TCP.ServerTypes ( ServerMessage(..), ServerModifier, ioConnector )
import TCP.Chan ( ShowRead, writeOutput, readInput )
import TCP.Message ( Message(..) )

import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

data NC name message = NamePrompt String | NC message
                               deriving (Show, Read)
instance (ShowRead name, ShowRead message) => ShowRead (NC name message)

data NS name message = MyNameIs name | NS message
                               deriving (Show, Read)
instance (ShowRead name, ShowRead message) => ShowRead (NS name message)

simpleNamedClient :: (ShowRead toclient, ShowRead toserver) =>
               String -> Client toclient toserver
            -> Client (NC String toclient) (NS String toserver)
simpleNamedClient n c =
    ioClient $ \intoclient ontoserver ->
        do inp <- readInput intoclient
           case inp of
             NamePrompt _ ->
                 do writeOutput ontoserver (MyNameIs n)
                    (i,o) <- forkClient c
                    forkIO $ forever $ do NC x <- readInput intoclient
                                          writeOutput o x
                    forever $ do x <- readInput i
                                 writeOutput ontoserver $ NS x
             _ -> fail "bad response from server!"

namedClient :: (ShowRead toclient, ShowRead toserver) =>
               Client toclient toserver
            -> Client (NC String toclient) (NS String toserver)
namedClient c =
    ioClient $ \intoclient ontoserver ->
        do inp <- readInput intoclient
           case inp of
             NamePrompt p ->
                 do putStrLn p
                    n <- getLine
                    writeOutput ontoserver (MyNameIs n)
                    (i,o) <- forkClient c
                    forkIO $ forever $ do NC x <- readInput intoclient
                                          writeOutput o x
                    forever $ do x <- readInput i
                                 writeOutput ontoserver $ NS x
             _ -> fail "bad response from server!"

pickNames :: (Eq client, ShowRead client, Eq name, ShowRead name,
              ShowRead toclient, ShowRead toserver) =>
             client -> name
          -> ServerModifier client (NC name toclient)
                                   (ServerMessage (NS name toserver))
                            name toclient (ServerMessage toserver)
pickNames upserver server = ioConnector $ \oup odown i ->
    do let handler xs =
             do m <- readInput i
                case m of
                  Left (Message x _ N) ->
                    do writeOutput oup $ Message upserver x
                                (NamePrompt "Welcome to our chat server!\nWhat is your name?")
                       handler xs
                  Left (Message x _ (M (MyNameIs n))) ->
                      case lookup x xs of
                        Just f -> do putStrLn ("Your name is already "++ show f)
                                     handler xs
                        Nothing -> do putStrLn ("New user "++show n)
                                      writeOutput odown (Message n server N)
                                      handler $ (x,n):xs
                  Left (Message x t (M (NS z))) ->
                      case lookup x xs of
                        Nothing -> do putStrLn ("Message from ... "++show x++
                                                " for "++show t)
                                      handler xs -- log bad message?
                        Just nf ->
                            case lookup t xs of
                              Nothing ->
                                  do putStrLn ("Message for "++show t++
                                               "? from "++ show nf)
                                     writeOutput odown (Message nf server (M z))
                                     handler xs
                              Just nt ->
                                  do putStrLn ("Message from "++show nf)
                                     writeOutput odown (Message nf nt (M z))
                                     handler xs
                  Right (Message fn tn z) ->
                      case rlookup fn xs of
                        Nothing -> do putStrLn ("foobar "++show fn)
                                      handler xs
                        Just f ->
                            case rlookup tn xs of
                              Nothing -> do putStrLn ("bazbar "++show tn)
                                            handler xs
                              Just t ->
                                  do writeOutput oup (Message f t (NC z))
                                     handler xs
       handler [(upserver,server)]

rlookup :: Eq a => a -> [(b,a)] -> Maybe b
rlookup x y = lookup x $ map (\ (a,b) -> (b,a)) y
