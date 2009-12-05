module Dominion.Message ( tell,
                          tellP, tellAll, tellOpp, tellOppOf, tellSelf ) where

import Dominion.Types

import TCP.Chan ( writeOutput )
import Control.Monad.State ( gets, liftIO )

tell :: PId -> String -> Game ()
tell p s = do och <- withPlayer p $ gets playerChan
              liftIO $ writeOutput och $ Info $ InfoMessage s

tellP :: PId -> InfoMessage -> Game ()
tellP p m = do och <- withPlayer p $ gets playerChan
               liftIO $ writeOutput och $ Info m

tellAll :: InfoMessage -> Game ()
tellAll m = getAllPlayers >>= mapM_ send
    where send p = do och <- withPlayer p $ gets playerChan
                      liftIO $ writeOutput och $ Info m

tellOpp :: InfoMessage -> Game ()
tellOpp m = getSelf >>= flip tellOppOf m

tellOppOf :: PId -> InfoMessage -> Game ()
tellOppOf p m = getOpponents p >>= mapM_ send
    where send p = do och <- withPlayer p $ gets playerChan
                      liftIO $ writeOutput och $ Info m

tellSelf :: InfoMessage -> Game ()
tellSelf m = getSelf >>= send
    where send p = do och <- withPlayer p $ gets playerChan
                      liftIO $ writeOutput och $ Info m
