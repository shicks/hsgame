module Dominion.Question where

import Dominion.Types
import Dominion.Unique ( newQId )

import Control.Concurrent.Chan ( writeChan )
import Control.Concurrent.MVar ( newEmptyMVar, takeMVar, putMVar )
import Control.Monad.State ( gets, liftIO )

-- *simple question with list of answers: choose 1
ask1 :: PId -> QuestionMessage -> [Answer] -> Game Answer
ask1 p _ [] = fail "ask1 requires nonempty choices"
ask1 p q as = do och <- gets $ playerChan . (!!p) . gamePlayers
                 ich <- gets inputChan
                 qid <- newQId
                 mv <- liftIO newEmptyMVar
                 let go = liftIO $ writeChan och $
                          Question qid q as (1,1)
                 liftIO $ writeChan ich $ RegisterQuestion qid $ \as' ->
                     case as' of
                       [a] | a `elem` as -> do liftIO $ putMVar mv a
                                               return True   -- unhook
                       _ -> go >> return False            -- keep hook
                 go
                 liftIO $ takeMVar mv

-- *simple question with list of cards: choose between m and n
askCards :: PId -> QuestionMessage -> (Int,Int) -> [Card] -> Game [Card]
askCards _ _ _ [] = return [] -- no cards -> no cards
askCards p q (mn,mx) cs =
    do och <- gets $ playerChan . (!!p) . gamePlayers
       ich <- gets inputChan
       qid <- newQId
       mv <- liftIO newEmptyMVar
       let go = liftIO $ writeChan och $
                Question qid q (map PickCard cs) (realMin,realMax)
           verify mv as | length as > realMax = go >> return False
                        | length as < realMin = go >> return False
                        | test as cs [] = do liftIO $ putMVar mv (map unCard as)
                                             return True
       liftIO $ writeChan ich $ RegisterQuestion qid $ verify mv
       go
       liftIO $ takeMVar mv
    where realMin = min mn $ length cs
          realMax = min mx $ length cs
          test [] _ _ = True -- as is subset of cs (unordered)
          test (PickCard c:as) (x:xs) ys
              | cardId c==cardId x = test as (xs++ys) []
              | otherwise = test (PickCard c:as) xs (x:ys)
          test _ _ _ = False
          unCard a = case a of { PickCard c -> c; _ -> error "impossible" }
