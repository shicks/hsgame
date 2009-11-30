||| Merge >>>
module Dominion.Cards where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question

import Control.Applicative ( pure, (<*>) )
import Control.Monad.State ( gets, modify )
import Control.Monad ( when, replicateM )
import Data.Maybe ( listToMaybe, catMaybes )
import Data.List ( nubBy )

-- import Control.Monad.Trans ( liftIO ) -- for debugging!

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

dominion :: [Card]
dominion = [chapel, cellar, feast, festival, laboratory, library,
            market, militia, mine, moat, remodel, smithy, thief,
            throneRoom, witch, woodcutter, workshop, village]

promos :: [Card]
promos = []

intrigue :: [Card]
intrigue = [harem, secretChamber]

seaside :: [Card]
seaside = [caravan]

-- cards themselves
chapel :: Card
chapel = Card 0 2 "Chapel" "Trash up to 4 cards from your hand" [Action a]
    where a _ = do (self,h,_,_) <- getSHGP
                   cs <- askCards self h (TrashBecause "chapel") (0,4)
                   mapM_ (flip trash $ hand self) cs

cellar :: Card
cellar = Card 0 2 "Cellar" "..." [Action a]
    where a _ = do (self,h,_,_) <- getSHGP
                   plusAction 1
                   cs <- askCards self h (DiscardBecause "cellar") (0,length h)
                   discard self *<<& (cs,hand self)
                   draw (length cs) self

feast :: Card
feast = Card 0 4 "Feast" "Trash this card.  Gain a card costing up to 5"
        [Action a]
    where a this = do (self,_,gain,_) <- getSHGP
                      trash this $ played
                      sup <- supplyCosting (<=5)
                      cs <- askCards self sup SelectGain (1,1)
                      case cs of [c] -> gain self c
                                 _ -> return ()

festival :: Card
festival = Card 0 5 "Festival" "..." $
          [Action $ \_ -> plusBuy 1 >> plusCoin 2 >> plusAction 2]

laboratory :: Card
laboratory = Card 0 5 "Laboratory" "..." $
          [Action $ \_ -> (getSelf >>= draw 2) >> plusAction 1]

library :: Card
library = Card 0 5 "Library" "..." [Action a]
    where a _ = do self <- getSelf
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

market :: Card
market = Card 0 5 "Market" "..." [Action a]
    where a _ = plusAction 1 >> plusBuy 1 >> (getSelf >>= draw 1) >> plusCoin 1

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
    where a _ = do (self,h,gain,price) <- getSHGP
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
moat = Card 0 2 "Moat" "..." [Action $ \_ -> getSelf >>= draw 2,Reaction r]
    where r _ _ = return $ \_ _ _ -> return ()

remodel :: Card
remodel = Card 0 4 "Remodel" "..." [Action a]
    where a _ = do (self,h,gain,price) <- getSHGP
                   cs <- askCards self h (TrashBecause "remodel") (1,1)
                   if null cs then return () else do
                   let c = head cs
                   trash c $ hand self
                   sup <- supplyCosting (<=(price c+2))
                   cs' <- askCards self sup SelectGain (1,1)
                   if null cs' then return () else gain self $ head cs'

smithy :: Card
smithy = Card 0 4 "Smithy" "..." [Action $ \_ -> getSelf >>= draw 3]

thief :: Card
thief = Card 0 4 "Thief" "..." [Action a]
    where a _ = attackNow "thief" $ \self opp -> do
                  cs <- catMaybes `fmap` replicateM 2 (top $ deck opp)
                  let treas = filter isTreasure cs
                  tc <- askCards self treas (TrashBecause "thief") (1,1)
                  discard opp *<<@ filter (not . (`elem`tc)) cs
                  case tc of
                    [c] -> do g <- ask1 self [Choose "Yes", Choose "No"] $
                                   OtherQuestion $ "Keep "++show c++"?"
                              when (g == Choose "Yes") $ discard self *<< c
                    _ -> return ()

throneRoom :: Card
throneRoom = Card 0 4 "Throne Room" "..." [Action a]
    where a _ = do (self,h,_,_) <- getSHGP
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
    where a _ = do getSelf >>= draw 2
                   attackNow "witch" $ \_ opp -> do gain <- gets hookGain
                                                    gain opp curse

woodcutter :: Card
woodcutter = Card 0 3 "Woodcutter" "..." [Action $ \_ -> plusCoin 2 >> plusBuy 1]

workshop :: Card
workshop = Card 0 3 "Workshop" "..." [Action a]
    where a _ = do (self,_,gain,_) <- getSHGP
                   sup <- supplyCosting (<=4)
                   cs <- askCards self sup SelectGain (1,1)
                   mapM_ (gain self) cs

village :: Card
village = Card 0 3 "Village" "..." $
          [Action $ \_ -> (getSelf >>= draw 1) >> plusAction 2]


-- *Intrigue

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
          act _ = do (self,h,_,_) <- getSHGP
                     cs <- askCards self h (DiscardBecause "secret chamber")
                           (0,length h)
                     discard self *<<& (cs,hand self)
                     plusCoin $ length cs

-- Seaside cards

caravan :: Card
caravan = Card 0 4 "Caravan" "..." [duration a]
    where a = do plusAction 1 >> plusCard 1
                 nextTurn $ plusCard 1

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


<<< Merge |||
||| finish implementing base set >>>
module Dominion.Cards where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question

import Control.Monad.State ( gets, modify, liftIO )
import Control.Monad.Error ( catchError )
import Control.Monad ( when, unless, join, replicateM, forM_  )
import Data.Maybe ( listToMaybe, maybeToList, catMaybes, fromMaybe )
import Data.List ( (\\), nubBy, partition )

-- import Control.Monad.Trans ( liftIO ) -- for debugging!

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

action :: Game () -> CardType
action act = Action $ \_ _ -> act

duration :: Game () -> CardType
duration act = Action $ \this pred -> do self <- getSelf
                                         let save = this:maybeToList pred
                                         durations self << save
                                         act

oneShot :: Game () -> CardType
oneShot act = Action $ \this _ -> do self <- getSelf
                                     trash << [this]
                                     act
dominion :: [Card]
dominion = [adventurer, bureaucrat, cellar, chancellor, chapel,
            councilRoom, feast, festival, gardens, laboratory,
            library, market, militia, mine, moat, moneylender,
            remodel, smithy, spy, thief, throneRoom, village,
            witch, woodcutter, workshop]

promos :: [Card]
promos = []

intrigue :: [Card]
intrigue = [courtyard, greatHall, harem, secretChamber]

seaside :: [Card]
seaside = [bazaar, caravan, fishingVillage, lookout, merchantShip,
           pearlDiver, salvager, tactician, warehouse, wharf]

-- cards themselves
adventurer :: Card
adventurer = Card 0 6 "Adventurer" "..." [action $ try $ dig 0 ]
    where dig 2 = return ()
          dig n = do self <- getSelf
                     [c] <-1<* deck self
                     if isTreasure c
                        then hand self << [c] >> dig (n+1)
                        else discard self *<< [c] >> dig n

bureaucrat :: Card
bureaucrat = Card 0 4 "Bureaucrat" "..." [action a]
    where a = do attackNow "bureaucrat" $ \self opp -> try $ do
                   h <- getStack $ hand opp
                   let vs = filter isVictory h
                   if null vs then return () else do -- reveal hand...?
                   [c] <- askCards opp vs (UndrawBecause "bureaucrat") (1,1)
                   deck opp *<< [c]
                 self <- getSelf
                 try $ gain self deck *<< [silver]

cellar :: Card
cellar = Card 0 2 "Cellar" "..." [action a]
    where a = do (self,h,_) <- getSHP
                 plusAction 1
                 cs <- askCards self h (DiscardBecause "cellar") (0,length h)
                 discard self *<< cs
                 draw (length cs) self

chancellor :: Card
chancellor = Card 0 3 "Chancellor" "..." [action a]
    where a = do self <- getSelf
                 plusCoin 2
                 shuf <- askYN self "Reshuffle deck?"
                 when shuf $ discard self *<<< deck self

chapel :: Card
chapel = Card 0 2 "Chapel" "Trash up to 4 cards from your hand" [action a]
    where a = do (self,h,_) <- getSHP
                 cs <- askCards self h (TrashBecause "chapel") (0,4)
                 trash << cs

councilRoom :: Card
councilRoom = Card 0 5 "Council Room" "..." [action a]
    where a = do plusCard 4
                 opp <- opponents
                 forM_ opp $ draw 1

feast :: Card
feast = Card 0 4 "Feast" "Trash this card.  Gain a card costing up to 5"
        [oneShot a]
    where a = do self <- getSelf
                 sup <- supplyCosting (<=5)
                 gain self discard *<# askCards self sup SelectGain (1,1)

festival :: Card
festival = Card 0 5 "Festival" "..." $ [action $ plusABCD 2 1 2 0]

gardens :: Card
gardens = Card 0 4 "Gardens" "..." $ [Victory, Score s]
    where s n = do self <- getSelf
                   c <- length `fmap` allCards self
                   return $ n + c`div`10

laboratory :: Card
laboratory = Card 0 5 "Laboratory" "..." $ [action $ plusABCD 1 0 0 2]

library :: Card
library = Card 0 5 "Library" "..." [action a]
    where a = do self <- getSelf
                 while $ do
                   h <- getStack $ hand self
                   when (length h >= 7) $ fail "done"
                   [c] <- top 1 $ deck self
                   if isAction c
                      then do keep <- askYN self $ "Keep "++show c++"?"
                              if keep then hand    self  << [c]
                                      else discard self *<< [c]
                      else do hand self << [c]           
                              tell self $ "Drew "++show c
          while job = catchError (job >> while job) (\_ -> return ())

market :: Card
market = Card 0 5 "Market" "..." [action $ plusABCD 1 1 1 1]

militia :: Card
militia = Card 0 4 "Militia" "..." [action a]
    where a = do plusCoin 2
                 attackNow "militia" $ \_ opp -> do
                   h <- getStack $ hand opp
                   let n = length h
                   when (n>3) $ do
                     cs <- askCards opp h (DiscardBecause "militia") (n-3,n-3)
                     discard opp *<< cs

mine :: Card
mine = Card 0 5 "Mine" "..." [action $ try a]
    where a = do (self,h,price) <- getSHP
                 [c] <- askCards self (filter isTreasure h)
                        (TrashBecause "mine") (1,1)
                 trash << [c]
                 sup <- filter isTreasure `fmap`
                        supplyCosting (<=(price c+3))
                 gain self hand <# askCards self sup SelectGain (1,1)

moat :: Card
moat = Card 0 2 "Moat" "..." [action $ plusCard 2,Reaction r]
    where r _ _ = return $ \_ _ _ -> return ()

moneylender :: Card
moneylender = Card 0 3 "Moneylender" "..." [action $ try a]
    where a = do (self,h,_) <- getSHP
                 [c] <- askCards self (filter (isNamed "Copper") h)
                        (TrashBecause "moneylender") (1,1)
                 trash << [c]
                 plusCoin 3

remodel :: Card
remodel = Card 0 4 "Remodel" "..." [action $ try a]
    where a = do (self,h,price) <- getSHP
                 [c] <- askCards self h (TrashBecause "remodel") (1,1)
                 trash << [c]
                 sup <- supplyCosting (<=(price c+2))
                 gain self discard *<# askCards self sup SelectGain (1,1)

smithy :: Card
smithy = Card 0 4 "Smithy" "..." [action $ plusCard 3]

spy :: Card
spy = Card 0 4 "Spy" "..." [action a]
    where a = do self <- getSelf
                 plusAction 1 >> plusCard 1
                 spy self self
                 attackNow "spy" spy
          spy self p = try $ do [c] <-1<* deck p
                                pname <- withPlayer p $ gets playerName
                                kill <- askYN self $ "(Player "++pname
                                        ++"): Discard "++show c++"?"
                                when kill $ discard p *<< [c]

thief :: Card
thief = Card 0 4 "Thief" "..." [action a]
    where a = attackNow "thief" $ \self opp -> try $ do
                cs <-2<* deck opp
                let (ts,nts) = partition isTreasure cs
                discard opp *<< nts
                [c] <- askCards self ts (TrashBecause "thief") (1,1)
                trash << [c]
                keep <- askYN self $ "Keep "++show c++"?"
                when keep $ gain self discard *<< [c]

-- TR and durations - FV=Fishing Village
-- card (pred)    [self is always self]
-- TR1()
-- TR2(TR1)            TR2()
-- FV1(TR1) FV1()    | TR3(TR2)         TR3()
--                     FV2(TR2) FV2() | FV3(TR3) FV3()
-- This idea of predecessors seems to get the job done.

throneRoom :: Card
throneRoom = Card 0 4 "Throne Room" "..." [Action a]
    where a this pred = try $ do (self,h,_) <- getSHP
                                 let as = filter isAction h
                                 [c] <- askCards self as SelectAction (1,1)
                                 played << [c]
                                 getAction c
                                 getActionPred (fromMaybe this pred) c

witch :: Card
witch = Card 0 5 "Witch" "..." [action a]
    where a = do plusCard 2
                 attackNow "witch" $ \_ opp -> gain opp discard *<< [curse]

woodcutter :: Card
woodcutter = Card 0 3 "Woodcutter" "..." [action $ plusABCD 0 1 2 0]

workshop :: Card
workshop = Card 0 3 "Workshop" "..." [action a]
    where a = do self <- getSelf
                 sup <- supplyCosting (<=4)
                 gain self discard *<# askCards self sup SelectGain (1,1)

village :: Card
village = Card 0 3 "Village" "..." $ [action $ plusABCD 2 0 0 1]


-- *Intrigue

courtyard :: Card
courtyard = Card 0 2 "Courtyard" "..." [action a]
    where a = do plusCard 3
                 (self,h,_) <- getSHP
                 deck self *<# askCards self h 
                               (UndrawBecause "courtyard") (1,1)

greatHall :: Card
greatHall = Card 0 3 "Great Hall" "..." [Victory, Score $ return.(1+),action a]
    where a = plusABCD 1 0 0 1

harem :: Card
harem = Card 0 6 "Harem" "2 Treasure, 2VP" [Victory, Treasure 2,
                                            Score $ return.(2+)]

secretChamber :: Card
secretChamber = Card 0 2 "Secret Chamber" "..." [action act,Reaction react]
    where react self cont = do draw 2 self
                               h <- getStack $ hand self
                               cs <- askCards self h
                                     (UndrawBecause "secret chamber") (2,2)
                               deck self *<< cs
                               cont
          act = do (self,h,_) <- getSHP
                   cs <- askCards self h (DiscardBecause "secret chamber")
                         (0,length h)
                   discard self *<< cs
                   plusCoin $ length cs

-- Seaside cards

bazaar :: Card
bazaar = Card 0 5 "Bazaar" "..." [action $ plusABCD 2 0 1 1]

caravan :: Card
caravan = Card 0 4 "Caravan" "..." [duration a]
    where a = plusABCD 1 0 0 1 >> nextTurn (plusCard 1)

fishingVillage :: Card
fishingVillage = Card 0 3 "Fishing Village" "..." [duration a]
    where a = plusABCD 2 0 1 0 >> nextTurn (plusABCD 1 0 1 0)

lookout :: Card
lookout = Card 0 3 "Lookout" "..." [action $ try a]
    where a = do self <- getSelf
                 plusAction 1
                 cs3 <-3<* deck self
                 -- if length cs < 3 then we go in order...
                 [t] <- askCards self cs3 (TrashBecause "lookout") (1,1)
                 trash << [t]
                 let cs2 = cs3 \\ [t]
                 [d] <- askCards self cs2 (DiscardBecause "lookout") (1,1)
                 discard self *<< [d]

merchantShip :: Card
merchantShip = Card 0 5 "Merchant Ship" "..." [duration a]
    where a = plusCoin 2 >> nextTurn (plusCoin 2)

pearlDiver :: Card
pearlDiver = Card 0 2 "Pearl Diver" "..." [action $ try a]
    where a = do self <- getSelf
                 plusAction 1 >> plusCard 1
                 [c] <-1<. deck self
                 move <- askYN self $ "Move " ++ show c ++ " to top?"
                 getStack (deck self) >>= ps "before"
                 if move then deck self *<< [c] else deck self .<< [c]
                 getStack (deck self) >>= ps "after"
          ps s x = liftIO $ putStrLn $ s ++ ": " ++ show x

salvager :: Card
salvager = Card 0 4 "Salvager" "..." [action a]
    where a = do (self,h,price) <- getSHP
                 plusBuy 1
                 trash <# mapM_ (plusCoin . price) #<#
                     askCards self h (TrashBecause "salvager") (1,1)

tactician :: Card
tactician = Card 0 5 "Tactician" "..." [duration a]
    where a = do (self,h,_) <- getSHP
                 unless (null h) $ nextTurn $ plusABCD 1 1 0 5
                 discard self *<<< hand self

-- smugglers - needs to hook into gain... :-/

warehouse :: Card
warehouse = Card 0 3 "Warehouse" "..." [action a]
    where a = do self <- getSelf
                 plusAction 1 >> draw 3 self
                 h <- getStack $ hand self
                 discard self *<#
                         askCards self h (DiscardBecause "warehouse") (3,3)

wharf :: Card
wharf = Card 0 5 "Wharf" "..." [duration a]
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

getTreasure :: Card -> Int -- this needs to be monadic: coppersmith!
getTreasure c = sum [t | Treasure t <- cardType c] 

isAction :: Card -> Bool
isAction c = not $ null [() | Action _ <- cardType c]

getAction :: Card -> Game ()
getAction c = foldl (>>) (return ()) [a c Nothing | Action a <- cardType c]

getActionPred :: Card -> Card -> Game ()
getActionPred pred c = foldl (>>) (return ())
                       [a c (Just pred) | Action a <- cardType c]

isNamed :: String -> Card -> Bool
isNamed name c = cardName c == name

-- Want some sort of asynchrony, in that we can go on with our
-- turn while others are selecting their reactions...
-- BUT, we need to also be able to wait for their reactions
-- and for the attack resolutions before going on if we
-- want to.


<<< finish implementing base set |||
