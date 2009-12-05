module Dominion.Question where

import Dominion.Types
import Dominion.Stack

import TCP.Chan ( writeOutput )
import Control.Concurrent.MVar ( newEmptyMVar, takeMVar, putMVar )
import Control.Monad.State ( gets, liftIO )
import Data.Maybe ( catMaybes, fromJust )

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

ask2 :: PId -> [Answer] -> QuestionMessage -> Game (Answer,Answer)
ask2 p as _ | length as<2 = fail "ask2 requires at least two choices"
ask2 p as q = do och <- withPlayer p $ gets playerChan
                 ich <- gets outputChan
                 qid <- newQId
                 liftIO $ do
                   mv <- newEmptyMVar
                   let go = writeOutput och $
                            Question qid q as (2,2)
                   writeOutput ich $ RQ qid $ \as' ->
                       case as' of
                         [a,a'] | a `elem` as && 
                                  a' `elem` as &&
                                  a /= a' -> do putMVar mv (a,a')
                                                return True   -- unhook
                         _ -> go >> return False            -- keep hook
                   go
                   takeMVar mv

-- *even simpler wrapper around ask1
askYN :: PId -> String -> Game Bool
askYN p s = do a <- ask1 p [Choose "Yes",Choose "No"] $ OtherQuestion s
               return $ case a of { Choose "Yes" -> True; _ -> False }

askMC :: PId -> [(String,Game a)] -> String -> Game a
askMC p ss s = do Choose a <- ask1 p (map (Choose . fst) ss) $
                              OtherQuestion s
                  fromJust $ lookup a ss

askMC2 :: PId -> [(String,Game ())] -> String -> Game ()
askMC2 p ss s = do (a,a') <- ask2 p (map (Choose . fst) ss) $
                             OtherQuestion s
                   mapM_ (\(Choose a) -> fromJust $ lookup a ss) [a,a']

-- *simple question with list of cards: choose between m and n
askCards :: PId -> [Card] -> QuestionMessage -> (Int,Int) -> Game [Card]
askCards = askCardsRepl $ \c -> filter (/= c)

-- *refinement of above, defaulting to own hand
askCardsHand :: QuestionMessage -> (Int,Int) -> Game [Card]
askCardsHand q r = do self <- getSelf
                      h <- getStack $ hand self
                      askCards self h q r

askCards' :: PId -> [Card] -> QuestionMessage -> (Int,Int) -> Game [Card]
askCards' = askCardsRepl (const id)

-- negative number for max means no maximum
askCardsRepl :: (Card -> [Card] -> [Card]) -- filter out card from list
             -> PId -> [Card] -> QuestionMessage -> (Int,Int) -> Game [Card]
askCardsRepl _ _ [] _ _ = return [] -- no cards -> no cards
askCardsRepl filt p cs q (mn,mx) =
    do och <- withPlayer p $ gets playerChan
       ich <- gets outputChan
       qid <- newQId
       cs' <- mapM fixPrice cs
       liftIO $ do
         mv <- newEmptyMVar
         let go = writeOutput och $
                  Question qid q (map pickCard cs') (realMin,realMax)
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
          realMax = if mx<0 || mx>length cs then length cs else mx
          test [] _ = True -- as is subset of cs (unordered)
          test (PickCard cd:as) cs =
              case lookupCard cd cs of
                Just c -> test as $ filt c cs
                Nothing -> False
          unCard a = case a of { PickCard c -> c; _ -> error "impossible" }
          fixPrice c = do p <- withTurn $ gets priceMod
                          return $ c { cardPrice = p c }
