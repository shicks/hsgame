module HTTP.LoginServer ( loginServer, loginThread, dirServer,
                          LoginMessage(SendMessage),
                          Agent(..)
                        ) where

import HTTP.Request ( Request(..), urlDecode )
import HTTP.Response ( Response, jsResponse, error404 )

import Control.Concurrent ( forkIO, threadDelay )
import TCP.Chan ( Input, Output, pipe, isEmptyInput,
                  writeOutput, readInput )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )
import Data.Maybe ( fromMaybe )
import Data.List ( groupBy )
import Data.Function ( on )
import Network.URI ( URI(..) )

-- The point is that we need a centralized place to store name/host
-- pairings, and then to pass only authenticated pairings along to
-- the other servers.  It would also be nice to have a single
-- polling system.

-- My idea is that we have an (Output Name) chan that gets passed
-- to all the other servers, and then the nameThread uses this to
-- answer questions, etc...  Or maybe somehow we can use this as a
-- sort of WRAPPER around the other servers...  That is,

newtype Agent = Agent { unAgent :: String } deriving ( Eq, Ord )
instance Show Agent where showsPrec _ (Agent s) = showString s
-- this isn't an inverse...
--instance Read Agent where readsPrec i = map (\(a,b) -> (Agent a,b))
--                                        . readsPrec i

type Hostname = String
data LoginMessage = SendMessage Agent String           -- script
                  | NewUser Agent Hostname (Bool -> IO ()) -- hostname
                  | Poll Agent Hostname (MVar (Maybe Response))
                  | Verify Agent Hostname (Bool -> IO ())
                           -- We could replace Verify with NewUser, but
                           -- this gives some added safety
                        -- -- | GetAgents (MVar [agent]) ...?
                        -- maybe stuff about disconnects, etc?

loginThread :: Input LoginMessage
            -> [(Agent,(Hostname,Input String,Output String))]
            -> IO ()
loginThread inp ag = do msg <- readInput inp
                        case msg of
                          NewUser a host ret ->
                              case lookup a ag of
                                Just (host',_,_)
                                    | host'==host -> do ret True
                                                        loginThread inp ag
                                    | otherwise   -> do ret False
                                                        loginThread inp ag
                                Nothing -> do (i,o) <- pipe
                                              ret True
                                              loginThread inp $
                                                  (a,(host,i,o)):ag
                          Verify a host ret -> do
                              case lookup a ag of
                                Just (host',_,_)
                                    | host'==host -> ret True
                                _ -> ret False
                              loginThread inp ag
                          SendMessage a s -> do
                              putStrLn $ "SendMessage "++show a++" "++show s
                              case lookup a ag of -- bad recipients?!?
                                Nothing -> putStrLn "NOT WRITING" >> return ()
                                Just (_,_,o) -> writeOutput o s
                              loginThread inp ag
                          Poll a host mv ->
                              do forkIO $ do
                                   case lookup a ag of
                                     Just (host',i,_)
                                       | host'==host -> emptyChan i mv
                                     _ -> do -- prevent accidental DOS
                                             threadDelay 5000000
                                             putMVar mv Nothing
                                 loginThread inp ag
    where emptyChan c mv = do putStrLn "Waiting for input on chan"
                              line <- readInput c
                              putStrLn $ "Got one line: "++line
                              rest <- ec' c
                              putMVar mv . Just =<< jsResponse
                                                    (unlines $ (line:rest))
          ec' c = do e <- isEmptyInput c
                     if e then return [] else do l <- readInput c
                                                 (l:) `fmap` ec' c

-- type AuthServer = (Agent -> [String] -> [(String,String)] -> IO Response)

dirServer :: [(Maybe String,        -- "directory name"
               (Agent -> [String] -> [(String,String)] -> IO Response))]
             -> Agent -> [String] -> [(String,String)] -> IO Response
dirServer spec u [] q = case lookup Nothing spec of
                          Nothing  -> error404
                          Just srv -> srv u [] q
dirServer spec u (p:ps) q = do putStrLn $ "dirServer: (p:ps)="++show (p:ps)
                               case lookup (Just p) spec of
                                 Nothing  -> putStrLn "404 error" >> error404
                                 Just srv -> putStrLn "found" >> srv u ps q

loginServer :: Output LoginMessage
            -> (Request -> IO Response)
            -> (Agent -> [String] -> [(String,String)] -> IO Response)
            -> (Request -> IO Response)
loginServer outp srv1 srv2 req =
      case rqURI req of
        Nothing  -> srv1 req
        Just uri -> let q = query uri in
            case lookup "u" q of
              Nothing -> srv1 req
              Just u' -> let u = Agent u' in
                  case paths uri of
                    ["login"] ->
                        do msuc <- newEmptyMVar
                           writeOutput outp $ NewUser u host (putMVar msuc)
                           suc <- takeMVar msuc
                           if suc then succR q
                                  else failR q
                    ["poll"] ->
                        do rsp <- newEmptyMVar
                           writeOutput outp $ Poll u host rsp
                           takeMVar rsp >>= maybe (relogR q) return
                    ps -> do mv <- newEmptyMVar
                             writeOutput outp $ Verify u host (putMVar mv)
                             ver <- takeMVar mv
                             if ver then srv2 u ps q
                                    else relogR q
    where query uri = urlDecode $ dropWhile (=='?') $ uriQuery uri
          host = rqFrom req
          paths uri = filter ((/="/").take 1) $
                      groupBy ((==)`on`(=='/')) $ uriPath uri
          failR = jsResponse . fromMaybe "$.login.fail();\n" . lookup "f"
          succR = jsResponse . fromMaybe "$.login.success();\n" . lookup "s"
          relogR = jsResponse . fromMaybe "$.login.relog();\n" . lookup "f"
