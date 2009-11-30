module Dominion.Cards where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question

import Control.Applicative ( pure, (<*>) )
import Control.Monad.State ( gets, modify )
import Control.Monad ( when, unless, join, replicateM, forM_  )
import Data.Maybe ( listToMaybe, catMaybes )
import Data.List ( (\\), nubBy )

-- import Control.Monad.Trans ( liftIO ) -- for debugging!

gain :: PId -> Card -> Game ()
gain p c = join $ (($c) . ($p)) `fmap` gets hookGain

getSHP :: Game (PId, [Card], Card -> Int) 
getSHP = do self <- gets currentTurn
            h <- getStack $ hand self
            price <- withTurn $ gets turnPriceMod
            return (self,h,price)

-- helper functions
affords :: Game (Int -> Card -> Bool)
affords = do price <- withTurn $ gets turnPriceMod
             return $ \p c -> price c <= p 

priceM :: Card -> Game Int
priceM c = do price <- withTurn $ gets turnPriceMod
              return $ price c

distinctSupplies :: Game [Card]
distinctSupplies = nubBy samename `fmap` allSupply
    where samename a b = cardName a == cardName b

supplyCosting :: (Int -> Bool) -> Game [Card]
supplyCosting f = do price <- withTurn $ gets turnPriceMod
                     sup <- distinctSupplies
                     return $ filter (f . price) sup

plusAction, plusBuy, plusCoin, plusCard :: Int -> Game ()
plusAction a = withTurn $ modify $ \s -> s { turnActions = a + turnActions s }
plusBuy b = withTurn $ modify $ \s -> s { turnBuys = b + turnBuys s }
plusCoin c = withTurn $ modify $ \s -> s { turnCoins = c + turnCoins s }
plusCard c = getSelf >>= draw c

plusABCD :: Int -> Int -> Int -> Int -> Game ()
plusABCD a b c d = do when (a>0) $ plusAction a
                      when (b>0) $ plusBuy b
                      when (c>0) $ plusCoin c
                      when (d>0) $ plusCard d -- "D"raw

-- deal with throne room later...!
nextTurn :: Game () -> Game ()
nextTurn later = do self <- getSelf
                    withPlayer self $ modify $
                                   \s -> s { durationEffects =
                                             later:durationEffects s }

duration :: Game () -> CardType
duration act = Action $ \this -> do self <- getSelf
                                    durations self *<<& ([this],played)
                                    act

oneShot :: Game () -> CardType
oneShot act = Action $ \this -> do self <- getSelf
                                   trash this $ played
                                   act
dominion :: [Card]
dominion = [chapel, cellar, feast, festival, laboratory, library,
            market, militia, mine, moat, remodel, smithy, thief,
            throneRoom, witch, woodcutter, workshop, village]

promos :: [Card]
promos = []

intrigue :: [Card]
intrigue = [courtyard, greatHall, harem, secretChamber]

seaside :: [Card]
seaside = [bazaar, caravan, fishingVillage, lookout, merchantShip,
           pearlDiver, salvager, tactician, warehouse, wharf]

-- cards themselves
chapel :: Card
chapel = Card 0 2 "Chapel" "Trash up to 4 cards from your hand" [Action a]
    where a _ = do (self,h,_) <- getSHP
                   cs <- askCards self h (TrashBecause "chapel") (0,4)
                   mapM_ (flip trash $ hand self) cs

cellar :: Card
cellar = Card 0 2 "Cellar" "..." [Action a]
    where a _ = do (self,h,_) <- getSHP
                   plusAction 1
                   cs <- askCards self h (DiscardBecause "cellar") (0,length h)
                   discard self *<<& (cs,hand self)
                   draw (length cs) self

feast :: Card
feast = Card 0 4 "Feast" "Trash this card.  Gain a card costing up to 5"
        [oneShot a]
    where a = do self <- getSelf
                 sup <- supplyCosting (<=5)
                 cs <- askCards self sup SelectGain (1,1)
                 case cs of [c] -> gain self c
                            _   -> return ()

festival :: Card
festival = Card 0 5 "Festival" "..." $ [Action $ \_ -> plusABCD 2 1 2 0]

laboratory :: Card
laboratory = Card 0 5 "Laboratory" "..." $ [Action $ \_ -> plusABCD 1 0 0 2]

library :: Card
library = Card 0 5 "Library" "..." [Action a]
    where a _ = do self <- getSelf
                   while $ do
                     h <- getStack $ hand self
                     if length h >= 7 then return False else do
                     mc <- top $ deck self
                     case mc of
                       Nothing -> return True
                       Just c
                         | isAction c -> do
                               keep <- askYN self $ "Keep "++show c++"?"
                               if keep then hand self *<< c
                                       else discard self *<< c
                               return True
                         | otherwise -> do
                               hand self *<< c
                               tell self $ "Drew "++show c
                               return True
          while job = do { result <- job; when result $ while job }

market :: Card
market = Card 0 5 "Market" "..." [Action $ \_ -> plusABCD 1 1 1 1]

militia :: Card
militia = Card 0 4 "Militia" "..." [Action a]
    where a _ = do plusCoin 2
                   attackNow "Militia" $ \_ opp -> do
                     h <- getStack $ hand opp
                     let n = length h
                     when (n>3) $ do
                       cs <- askCards opp h (DiscardBecause "militia") (n-3,n-3)
                       discard opp *<<& (cs,hand opp)

mine :: Card
mine = Card 0 5 "Mine" "..." [Action a]
    where a _ = do (self,h,price) <- getSHP
                   cs <- askCards self (filter isTreasure h)
                         (TrashBecause "mine") (1,1)
                   if null cs then return () else do
                   let c = head cs
                   trash c $ hand self
                   sup <- filter isTreasure `fmap` supplyCosting (<=(price c+3))
                   cs' <- askCards self sup SelectGain (1,1)
                   if null cs' then return () else do gain self $ head cs'
                                                      hand self *<<* discard self

moat :: Card
moat = Card 0 2 "Moat" "..." [Action $ \_ -> plusCard 2,Reaction r]
    where r _ _ = return $ \_ _ _ -> return ()

remodel :: Card
remodel = Card 0 4 "Remodel" "..." [Action a]
    where a _ = do (self,h,price) <- getSHP
                   cs <- askCards self h (TrashBecause "remodel") (1,1)
                   if null cs then return () else do
                   let c = head cs
                   trash c $ hand self
                   sup <- supplyCosting (<=(price c+2))
                   cs' <- askCards self sup SelectGain (1,1)
                   if null cs' then return () else gain self $ head cs'

smithy :: Card
smithy = Card 0 4 "Smithy" "..." [Action $ \_ -> plusCard 3]

thief :: Card
thief = Card 0 4 "Thief" "..." [Action a]
    where a _ = attackNow "thief" $ \self opp -> do
                  cs <- catMaybes `fmap` replicateM 2 (top $ deck opp)
                  let treas = filter isTreasure cs
                  tc <- askCards self treas (TrashBecause "thief") (1,1)
                  discard opp *<<@ filter (not . (`elem`tc)) cs
                  case tc of
                    [c] -> do keep <- askYN self $ "Keep "++show c++"?"
                              when keep $ discard self *<< c
                    _ -> return ()

throneRoom :: Card
throneRoom = Card 0 4 "Throne Room" "..." [Action a]
    where a _ = do (self,h,_) <- getSHP
                   let as = filter isAction h
                   cs <- askCards self as SelectAction (1,1)
                   case cs of [c] -> do played *<<& (cs,hand self)
                                        getAction c
                                        getAction c
                              _ -> return ()

          -- There are some subtleties with durations here, but we'll worry
          -- about that later...

witch :: Card
witch = Card 0 5 "Witch" "..." [Action a]
    where a _ = do plusCard 2
                   attackNow "witch" $ \_ opp -> do gain <- gets hookGain
                                                    gain opp curse

woodcutter :: Card
woodcutter = Card 0 3 "Woodcutter" "..." [Action $ \_ -> plusABCD 0 1 2 0]

workshop :: Card
workshop = Card 0 3 "Workshop" "..." [Action a]
    where a _ = do self <- getSelf
                   sup <- supplyCosting (<=4)
                   cs <- askCards self sup SelectGain (1,1)
                   mapM_ (gain self) cs

village :: Card
village = Card 0 3 "Village" "..." $ [Action $ \_ -> plusABCD 2 0 0 1]


-- *Intrigue

courtyard :: Card
courtyard = Card 0 2 "Courtyard" "..." [Action a]
    where a _ = do plusCard 3
                   (self,h,_) <- getSHP
                   cs <- askCards self h (UndrawBecause "courtyard") (1,1)
                   deck self *<<& (cs,hand self)

greatHall :: Card
greatHall = Card 0 3 "Great Hall" "..." [Victory, Score $ return.(1+),Action a]
    where a _ = plusABCD 1 0 0 1

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
          act _ = do (self,h,_) <- getSHP
                     cs <- askCards self h (DiscardBecause "secret chamber")
                           (0,length h)
                     discard self *<<& (cs,hand self)
                     plusCoin $ length cs

-- Seaside cards

bazaar :: Card
bazaar = Card 0 5 "Bazaar" "..." [Action $ \_ -> plusABCD 2 0 1 1]

caravan :: Card
caravan = Card 0 4 "Caravan" "..." [duration a]
    where a = plusABCD 1 0 0 1 >> nextTurn (plusCard 1)

fishingVillage :: Card
fishingVillage = Card 0 3 "Fishing Village" "..." [duration a]
    where a = plusABCD 2 0 1 0 >> nextTurn (plusABCD 1 0 1 0)

lookout :: Card
lookout = Card 0 3 "Lookout" "..." [Action a]
    where a _ = do self <- getSelf
                   plusAction 1
                   cs3 <- catMaybes `fmap` (replicateM 3 $ top $ deck self)
                   -- if length cs < 3 then we go in order...
                   ct <- askCards self cs3 (TrashBecause "lookout") (1,1)
                   forM_ ct $ \c -> trash c $ deck self
                   let cs2 = cs3 \\ ct
                   cd <- askCards self cs2 (DiscardBecause "lookout") (1,1)
                   discard self *<<& (cd,deck self)
                   deck self *<<@ (cs2 \\ cd) -- move back to top...

merchantShip :: Card
merchantShip = Card 0 5 "Merchant Ship" "..." [duration a]
    where a = plusCoin 2 >> nextTurn (plusCoin 2)

pearlDiver :: Card
pearlDiver = Card 0 2 "Pearl Diver" "..." [Action a]
    where a _ = do self <- getSelf
                   plusAction 1 >> plusCard 1
                   mc <- bottom $ deck self
                   case mc of
                     Nothing -> return ()
                     Just c -> do
                       move <- askYN self $ "Move " ++ show c ++ " to top?"
                       if move then deck self *<< c else deck self .<< c

salvager :: Card
salvager = Card 0 4 "Salvager" "..." [Action a]
    where a _ = do (self,h,price) <- getSHP
                   plusBuy 1
                   ct <- askCards self h (TrashBecause "salvager") (1,1)
                   forM_ ct $ \c -> do plusCoin $ price c
                                       trash c $ hand self
                                       
tactician :: Card
tactician = Card 0 5 "Tactician" "..." [duration a]
    where a = do (self,h,_) <- getSHP
                 unless (null h) $ nextTurn $ plusABCD 1 1 0 5
                 discard self *<<< hand self

-- smugglers - needs to hook into gain... :-/

warehouse :: Card
warehouse = Card 0 3 "Warehouse" "..." [Action a]
    where a _ = do self <- getSelf
                   plusAction 1 >> draw 3 self
                   h <- getStack $ hand self
                   cd <- askCards self h (DiscardBecause "warehouse") (3,3)
                   discard self *<<& (cd,hand self)

wharf :: Card
wharf = Card 0 5"Wharf" "..." [duration a]
    where a = plusABCD 0 1 0 2 >> nextTurn (plusABCD 0 1 0 2)

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
getAction c = foldl (>>) (return ()) [a c | Action a <- cardType c]





-- Want some sort of asynchrony, in that we can go on with our
-- turn while others are selecting their reactions...
-- BUT, we need to also be able to wait for their reactions
-- and for the attack resolutions before going on if we
-- want to.

