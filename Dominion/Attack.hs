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
