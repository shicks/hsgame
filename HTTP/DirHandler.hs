module HTTP.DirHandler ( dirHandler ) where

import HTTP.Response ( Response, error404 )
import HTTP.LoginServer ( Agent )
import HTTP.Handlers ( Handler(..), useHandler )

dirHandler :: [(Maybe String, IO Handler)]
           -> IO (Agent -> [String] -> [(String,String)] -> IO Response)
dirHandler spec0 = do spec <- mapM makeHandler spec0
                      return (dirServer spec)
    where makeHandler (ms, mkh) = do h <- mkh >>= useHandler
                                     return (ms, h)

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
