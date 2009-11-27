module NamePicker ( pickNames ) where

import TCP.ServerTypes ( ServerMessage(..), ServerModifier, ioConnector )
import TCP.Chan ( ShowRead, writeOutput, readInput )
import TCP.Message ( Message(..) )

data NC name message = NamePrompt String | NC message
                               deriving (Show, Read)
instance (ShowRead name, ShowRead message) => ShowRead (NC name message)

data NS name message = MyNameIs name | NS message
                               deriving (Show, Read)
instance (ShowRead name, ShowRead message) => ShowRead (NS name message)

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
                                (NamePrompt "Welcome to our chat server!")
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
