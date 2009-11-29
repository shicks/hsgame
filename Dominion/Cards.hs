module Dominion.Cards where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question

import Control.Applicative ( pure, (<*>) )
import Control.Monad.State ( gets, modify )
import Control.Monad ( when, replicateM )
import Data.Maybe ( listToMaybe, catMaybes )


import Control.Monad.Trans ( liftIO ) -- for debugging!

getSHGP :: Game (PId, [Card], PId -> Card -> Game (), Card -> Int) 
getSHGP = do self <- gets currentTurn
             h <- getStack $ hand self
             gain <- gets hookGain
             price <- withTurn $ gets turnPriceMod
             return (self,h,gain,price)

-- helper functions
affords :: Game (Int -> Card -> Bool)
affords = do price <- withTurn $ gets turnPriceMod
             return $ \p c -> price c <= p 

supplyCosting :: (Int -> Bool) -> Game [Card]
supplyCosting f = do price <- withTurn $ gets turnPriceMod
                     sup <- gets gameSupply
                     return $ concat [if n>0 && f (price c)
                                      then [c] else [] | (c,n) <- sup]

plusAction, plusBuy, plusCoin :: Int -> Game ()
plusAction a = withTurn $ modify $ \s -> s { turnActions = a + turnActions s }
plusBuy b = withTurn $ modify $ \s -> s { turnBuys = b + turnBuys s }
plusCoin c = withTurn $ modify $ \s -> s { turnCoins = c + turnCoins s }

-- cards themselves
chapel :: Card
chapel = Card 0 2 "Chapel" "Trash up to 4 cards from your hand" [Action a]
    where a = do (self,h,_,_) <- getSHGP
                 cs <- askCards self h (TrashBecause "chapel") (0,4)
                 mapM_ (flip trash $ hand self) cs

cellar :: Card
cellar = Card 0 2 "Cellar" "..." [Action a]
    where a = do (self,h,_,_) <- getSHGP
                 plusAction 1
                 cs <- askCards self h (DiscardBecause "cellar") (0,length h)
                 discard self *<<& (cs,hand self)
                 draw (length cs) self

library :: Card
library = Card 0 5 "Library" "..." [Action a]
    where a = do self <- getSelf
                 let drw = do h <- getStack $ hand self
                              if length h >= 7 then return () else do
                              mc <- top $ deck self
                              case mc of
                                Nothing -> return ()
                                Just c
                                    | isAction c -> do
                                       resp <- askKeep c
                                       case resp of
                                         Choose "Yes" -> hand self *<< c
                                         _ -> discard self *<< c
                                    | otherwise -> do
                                        hand self *<< c
                                        tell self $ "Drew "++show c
                              drw
                     askKeep c = ask1 self [Choose "Yes",Choose "No"] $
                                 OtherQuestion $ "Keep "++show c++"?"
                 drw

moat :: Card
moat = Card 0 2 "Moat" "..." [Action $ getSelf >>= draw 2,Reaction r]
    where r _ _ = return $ \_ _ _ -> return ()

militia :: Card
militia = Card 0 4 "Militia" "..." [Action a]
    where a = do plusCoin 2
                 attackNow "Militia" $ \_ opp -> do
                   h <- getStack $ hand opp
                   let n = length h
                   when (n>3) $ do
                     cs <- askCards opp h (DiscardBecause "militia") (n-3,n-3)
                     discard opp *<<& (cs,hand opp)

village :: Card
village = Card 0 3 "Village" "..." $
          [Action $ (getSelf >>= draw 1) >> plusAction 2]

workshop :: Card
workshop = Card 0 3 "Workshop" "..." [Action a]
    where a = do (self,_,gain,_) <- getSHGP
                 sup <- supplyCosting (<=4)
                 cs <- askCards self sup SelectGain (1,1)
                 mapM_ (gain self) cs
                 -- pickFromSupply self aff SelectGain $ self . gain

woodcutter :: Card
woodcutter = Card 0 3 "Woodcutter" "..." [Action $ plusCoin 2 >> plusBuy 1]

-- now :: m (a -> b) -> a -> m b
-- now f a = f >>= ($a)

remodel :: Card
remodel = Card 0 4 "Remodel" "..." [Action a]
    where a = do (self,h,gain,price) <- getSHGP
                 cs <- askCards self h (TrashBecause "remodel") (1,1)
                 if null cs then return () else do
                 let c = head cs
                 trash c $ hand self
                 sup <- supplyCosting (<=(price c+2))
                 cs' <- askCards self sup SelectGain (1,1)
                 if null cs' then return () else gain self $ head cs'

smithy :: Card
smithy = Card 0 4 "Smithy" "..." [Action $ getSelf >>= draw 3]

market :: Card
market = Card 0 5 "Market" "..." [Action a]
    where a = plusAction 1 >> plusBuy 1 >> (getSelf >>= draw 1) >> plusCoin 1

mine :: Card
mine = Card 0 5 "Mine" "..." [Action a]
    where a = do (self,h,gain,price) <- getSHGP
                 cs <- askCards self (filter isTreasure h) (TrashBecause "mine")
                       (1,1)
                 if null cs then return () else do
                 let c = head cs
                 trash c $ hand self
                 sup <- filter isTreasure `fmap` supplyCosting (<=(price c+3))
                 cs' <- askCards self sup SelectGain (1,1)
                 if null cs' then return () else do gain self $ head cs'
                                                    hand self *<<* discard self

thief :: Card
thief = Card 0 4 "Thief" "..." [Action a]
    where a = attackNow "thief" $ \self opp -> do
                cs <- catMaybes `fmap` replicateM 2 (top $ deck opp)
                let treas = filter isTreasure cs
                tc <- askCards self treas (TrashBecause "thief") (1,1)
                discard opp *<<@ filter (not . (`elem`tc)) cs
                case tc of
                  [c] -> do g <- ask1 self [Choose "Yes", Choose "No"] $
                                 OtherQuestion $ "Keep "++show c++"?"
                            discard self *<< c
                  _ -> return ()

-- thiefAttack :: Game ()
-- thiefAttack = do opp <- attack "thief"
--                  opp $ \attacker defender -> do
--                    cs <- defender $ draw 2
--                    cs' <- concat `fmap` mapM (\c -> if isTreasure c then return [c] else defender (gain c) >> return []) cs
--                    unless (null cs') $ do
--                      [tc] <- attacker $ askCards cs' (TrashBecause "thief") (1,1)
--                      gain <- attacker $ ask $ ThiefGain [True,False]
--                      when gain $ attacker $ gain tc
--                      -- defender gains otherwise...




harem :: Card
harem = Card 0 6 "Harem" "2 Treasure, 2VP" [Victory, Treasure 2,
                                            Score $ return.(2+)]

secretChamber :: Card
secretChamber = Card 0 2 "Secret Chamber" "..." [Action act,Reaction react]
    where react self cont = do draw 2 self
                               h <- getStack $ hand self
                               cs <- askCards self h
                                     (UndrawBecause "secret chamber") (2,2)
                               deck self *<<& (cs,hand self)
                               cont
          act = do (self,h,_,_) <- getSHGP
                   cs <- askCards self h (DiscardBecause "secret chamber")
                         (0,length h)
                   discard self *<<& (cs,hand self)
                   plusCoin $ length cs


-- Basic cards

estate :: Card
estate = Card 0 2 "Estate" "1VP" [Victory, Score $ return.(1+)]

duchy :: Card
duchy = Card 0 5 "Duchy" "3VP" [Victory, Score $ return.(3+)]

province :: Card
province = Card 0 8 "Province" "6VP" [Victory, Score $ return.(6+)]

copper :: Card
copper = Card 0 0 "Copper" "1 coin" [Treasure 1]

silver :: Card
silver = Card 0 3 "Silver" "2 coins" [Treasure 2]

gold :: Card
gold = Card 0 6 "Gold" "3 coins" [Treasure 3]

curse :: Card
curse = Card 0 0 "Curse" "-1VP" [Score $ return.(-1+)]




isVictory :: Card -> Bool
isVictory c = not $ null [() | Treasure _ <- cardType c]

isTreasure :: Card -> Bool
isTreasure c = not $ null [() | Treasure _ <- cardType c]

getTreasure :: Card -> Int
getTreasure c = sum [t | Treasure t <- cardType c] 

isAction :: Card -> Bool
isAction c = not $ null [() | Action _ <- cardType c]

getAction :: Card -> Game ()
getAction c = foldl (>>) (return ()) [a | Action a <- cardType c]





-- Want some sort of asynchrony, in that we can go on with our
-- turn while others are selecting their reactions...
-- BUT, we need to also be able to wait for their reactions
-- and for the attack resolutions before going on if we
-- want to.

