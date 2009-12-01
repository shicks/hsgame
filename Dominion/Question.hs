module Dominion.Question where

import Dominion.Types

import TCP.Chan ( writeOutput )
import Control.Concurrent.MVar ( newEmptyMVar, takeMVar, putMVar )
import Control.Monad.State ( gets, liftIO )
import Data.Maybe ( catMaybes )

tell :: PId -> String -> Game ()
tell p s = do och <- withPlayer p $ gets playerChan
              liftIO $ writeOutput och $ Info $ InfoMessage s

-- *simple question with list of answers: choose 1
ask1 :: PId -> [Answer] -> QuestionMessage -> Game Answer
ask1 p [] _ = fail "ask1 requires nonempty choices"
ask1 p as q = do och <- withPlayer p $ gets playerChan
                 ich <- gets outputChan
                 qid <- newQId
                 liftIO $ do
                   mv <- newEmptyMVar
                   let go = writeOutput och $
                            Question qid q as (1,1)
                   writeOutput ich $ RQ qid $ \as' ->
                       case as' of
                         [a] | a `elem` as -> do putMVar mv a
                                                 return True   -- unhook
                         _ -> go >> return False            -- keep hook
                   go
                   takeMVar mv

-- *even simpler wrapper around ask1
askYN :: PId -> String -> Game Bool
askYN p s = do a <- ask1 p [Choose "Yes",Choose "No"] $ OtherQuestion s
               return $ case a of { Choose "Yes" -> True; _ -> False }

-- *simple question with list of cards: choose between m and n
askCards :: PId -> [Card] -> QuestionMessage -> (Int,Int) -> Game [Card]
askCards _ [] _ _ = return [] -- no cards -> no cards
askCards p cs q (mn,mx) =
    do och <- withPlayer p $ gets playerChan
       ich <- gets outputChan
       qid <- newQId
       liftIO $ do
         mv <- newEmptyMVar
         let go = writeOutput och $
                  Question qid q (map pickCard cs) (realMin,realMax)
             verify mv as | length as > realMax = go >> return False
                          | length as < realMin = go >> return False
                          | test as cs = do putMVar mv $ catMaybes $
                                             map ((`lookupCard` cs) . unCard) as
                                            return True
                          | otherwise = go >> return False
         writeOutput ich $ RQ qid $ verify mv
         go
         takeMVar mv
    where realMin = min mn $ length cs
          realMax = min mx $ length cs
          test [] _ = True -- as is subset of cs (unordered)
          test (PickCard cd:as) cs =
              case lookupCard cd cs of
                Just c -> test as (filter (/= c) cs)
                Nothing -> False
          unCard a = case a of { PickCard c -> c; _ -> error "impossible" }
