module Dominion.Question where

import Dominion.Types
import Dominion.Unique ( newQId )

import Control.Concurrent.Chan ( writeChan )
import Control.Concurrent.MVar ( newEmptyMVar, takeMVar, putMVar )

-- *simple question with list of answers: choose 1
ask1 :: PId -> String -> [Answer] -> Game Answer
ask1 p _ [] = fail "ask1 requires nonempty choices"
ask1 p s as = do och <- gets $ playerChan . (!!p) . players
                 ich <- gets inputChan
                 qid <- newQId
                 mv <- newEmptyMVar
                 let go = liftIO $ writeChan och $
                          Question qid s as (1,1)
                 liftIO $ writeChan ich $ RegisterQuestion qid $ \as' ->
                     case as' of
                       [a] | a `elem` as -> do liftIO $ putMVar mv a
                                               return True   -- unhook
                       _ -> go >> return False            -- keep hook
                 go
                 takeMVar mv

-- *simple question with list of cards: choose between m and n
askCards :: PId -> String -> (Int,Int) -> [Card] -> Game [Card]
askCards _ _ [] _ = return [] -- no cards -> no cards
askCards p s (mn,mx) cs = do och <- gets $ playerChan . (!!p) . players
                             ich <- gets inputChan
                             qid <- newQId
                             mv <- newEmptyMVar
                             let go = liftIO $ writeChan och $
                                      Question qid s (map CardPicked cs)
                                                   (realMin,realMax)
                             liftIO $ writeChan ich $
                                    RegisterQuestion qid $ verify mv
                             go
                             takeMVar mv
    where realMin = min mn $ length cs
          realMax = min mx $ length cs
          verify mv as | length as > realMax = go >> return False
                       | length as < realMin = go >> return False
                       | test as cs [] = do putMVar mv (map unCard as)
                                            return True
          test [] _ _ = True -- as is subset of cs (unordered)
          test (PickCard c:as) (x:xs) ys
              | cardId c==cardId x = test as (xs++ys) []
              | otherwise = test (PickCard c:as) xs (x:ys)
          test _ _ _ = False
          unCard a = case a of { PickCard c -> c; _ -> error "impossible" }
