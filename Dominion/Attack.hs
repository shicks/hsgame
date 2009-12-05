module Dominion.Attack ( attack, attackNow ) where

import Dominion.Types
import Dominion.Question
import Dominion.Stack
import Dominion.Message ( tellAll )

import Control.Monad.Error ( catchError )
import Control.Monad.State ( gets )

isReaction :: Card -> Bool
isReaction c = not $ null [() | Reaction _ <- cardType c]

getReaction :: Card -> Reaction -- unsafe!
getReaction c = head [r | Reaction r <- cardType c]

isDurationReaction :: Card -> Bool
isDurationReaction c = not $ null [() | DReaction _ <- cardType c]

getDurationReaction :: Card -> Reaction -- unsafe!
getDurationReaction c = head [r | DReaction r <- cardType c]

-- put some stuff here to say whether order matters, once we start
-- parallelizing the attacks.

attack :: String -> Game (Attack -> Game ())
attack name = do self <- getSelf
                 n <- gets $ length . gamePlayers
                 let opps = filter (/=self) $ map PId [0..n-1]
                 rs <- mapM (react name []) opps
                 let att a = mapM_ (\(p,r) -> r a self p) $ zip opps rs
                 return att

react :: String -> [Card] -> PId -> Game (Attack -> Attack)
react a cs p = do ds <- filter isDurationReaction `fmap` getStack (durations p)
                  case filter (not . (`elem`cs)) ds of
                     d:ds' -> getDurationReaction d p (react a (d:cs) p)
                     [] -> do
                       h <- filter isReaction `fmap` getStack (hand p)
                       let rs = filter (not . (`elem`cs)) ds
                       catchError `flip` (\_ -> return id) $ do
                         [r] <- askCards p rs (SelectReaction a)
                                (0,1)
                         name <- withPlayer p $ gets playerName
                         tellAll $ CardReveal name [describeCard r] "hand"
                         getReaction r p (react a (r:cs) p)

attackNow :: String -> Attack -> Game ()
attackNow s a = attack s >>= ($a)

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
