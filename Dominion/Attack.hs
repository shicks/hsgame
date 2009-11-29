||| Merge >>>
module Dominion.Attack where

import Dominion.Types

-- *How to actually perform the attack.  This is slightly tricky, since
-- some attacks depend on choices made by attacker...
type Attack = PId      -- ^attacker
            -> PId     -- ^defender
            -> Game ()

-- *Basic reaction type is @Attack -> Attack@.  But @Reaction@ type asks
-- the attacked player what to do, and gives a continuation in case the
-- attack is still unresolved.  @Duration@s can "install" @Reaction@s as
-- well.
type Reaction = PId                       -- ^defender
              -> Game (Attack -> Attack)  -- ^continuation
              -> Game (Attack -> Attack)

attack :: String -> Game (Attack -> Game ())
attack name a = do t <- gets currentTurn
                   let react :: (Int,PlayerState)
                             -> Game [Attack -> Attack]
                       react (i,p)
                           | i==t      = return []
                           | otherwise = do
                         let o = playerToGame i
                         rx <- pickFromHand o isReaction
                               (SelectReaction name) $ \c -> do
                                 let rxn = getReaction c
                                 rxn
                   rs <- mapM react =<< (gets $ zip [0..] . players)

askReaction :: PlayerId -> String -> Game (Attack -> Attack)
askReaction p s = do h <- getStack $ PlayerStack p Hand
                     let react :: [Card] -> Game (Attack -> Attack)
                         react [] = return id
                         react rs = askCards p rs (ReactTo s) (0,1) $ \cs ->
                                    case cs of
                                      []  -> return id
                                      [c] -> let rc = getReaction c
                                                 cs' = filter (/=c) cs
                                             in rc p (react cs')
                     react $ filter isReaction h

<<< Merge |||
||| Merge stupidly? >>>
module Dominion.Attack ( attack, attackNow ) where

import Dominion.Types
import Dominion.Question
import Dominion.Stack

import Control.Monad.State ( gets )

isReaction :: Card -> Bool
isReaction c = not $ null [() | Reaction _ <- cardType c]

getReaction :: Card -> Reaction -- unsafe!
getReaction c = head [r | Reaction r <- cardType c]

attack :: String -> Game (Attack -> Game ())
attack name = do self <- getSelf
                 n <- gets $ length . gamePlayers
                 let opps = filter (/=self) $ map PId [0..n-1]
                 let react :: PId -> Game (Attack -> Attack)
                     react p = do h <- getStack $ hand p
                                  let rs = filter isReaction h
                                  r <- askCards p rs (SelectReaction name)
                                       (0,1)
                                  if null r then return id else do
                                  getReaction (head r) p (react p)
                 rs <- mapM react opps
                 let att a = mapM_ (\(p,r) -> r a self p) $ zip opps rs
                 return att

attackNow :: String -> Attack -> Game ()
attackNow s a = do att <- attack s
                   att a

-- askReaction :: PlayerId -> String -> Game (Attack -> Attack)
-- askReaction p s = do h <- getStack $ PlayerStack p Hand
--                      let react :: [Card] -> Game (Attack -> Attack)
--                          react [] = return id
--                          react rs = askCards p rs (ReactTo s) (0,1) $ \cs ->
--                                     case cs of
--                                       []  -> return id
--                                       [c] -> let rc = getReaction c
--                                                  cs' = filter (/=c) cs
--                                              in rc p (react cs')
--                      react $ filter isReaction h

<<< Merge stupidly? |||
