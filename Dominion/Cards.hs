module Dominion.Cards where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question

import Control.Monad.State ( gets, modify, liftIO )
import Control.Monad.Error ( catchError )
import Control.Monad ( when, unless, join, replicateM, forM, forM_,
                       zipWithM_ )
import Data.Maybe ( listToMaybe, maybeToList, catMaybes, fromMaybe )
import Data.List ( (\\), nubBy, partition )
import Data.Function ( on )

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
distinctSupplies = nubBy sameName `fmap` allSupply

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

forceDiscard :: String -> PId -> Int -> Game ()
forceDiscard s p n = do h <- getStack $ hand p
                        discard p *<# askCards p h (DiscardBecause s) (n,n)

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
promos = [blackMarket, envoy]

intrigue :: [Card]  -- coppersmith
intrigue = [baron, bridge, conspirator, courtyard,
            duke, greatHall, harem, ironworks, masquerade,
            miningVillage, minion, nobles, pawn, saboteur,
            scout, secretChamber, shantyTown, steward,
            swindler, torturer, tradingPost, tribute,
            upgrade, wishingWell]

seaside :: [Card]  -- outpost, treasury, smugglers
seaside = [bazaar, caravan, fishingVillage, lookout, merchantShip,
           pearlDiver, salvager, tactician, warehouse, wharf]

allDecks :: [Card]
allDecks = dominion ++ promos ++ intrigue ++ seaside

-- cards themselves
adventurer :: Card
adventurer = Card 0 6 "Adventurer" "..." [action $ try $ dig 0 ]
    where dig 2 = return ()
          dig n = do self <- getSelf
                     [c] <-1<* deck self
                     if isTreasure c
                        then do hand self << [c]
                                dig (n+1)
                        else do discard self *<< [c]
                                dig n

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
                 getSelf >>= getOpponents >>= mapM_ (draw 1)

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
                   when (n>3) $ forceDiscard "militia" opp (n-3)

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
                 [c] <- askCards self (filter (sameName copper) h)
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
                 spy (\_ -> "Self") self self
                 attackNow "spy" $ spy ("Opponent "++)
          spy fmt self p = try $ do
                               [c] <-1<* deck p
                               who <- withPlayer p $ fmt `fmap` gets playerName
                               let msg = "("++who++"): Discard "++show c++"?"
                                   choices = [("Discard",discard p *<< [c]),
                                              ("Keep",return ())]
                               askMC self choices msg

thief :: Card
thief = Card 0 4 "Thief" "..." [action a]
    where a = attackNow "thief" $ \self opp -> try $ do
                cs <-2<* deck opp
                let (ts,nts) = partition isTreasure cs
                discard opp *<< nts
                [c] <- askCards self ts (TrashBecause "thief") (1,1)
                askMC self [("Steal",gain self discard *<< [c]),
                            ("Trash",trash << [c])] $ "Steal "++show c++"?"

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

village :: Card
village = Card 0 3 "Village" "..." $ [action $ plusABCD 2 0 0 1]

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


-- *Promo cards
blackMarket :: Card
blackMarket = Card 0 3 "Black Market" "..." [Hook (SetupHook setup), action a]
    where bmDeck = orderedStack $ SN "blackMarket"
          notIn cs c = not $ cardName c `elem` map cardName cs
          setup cs = bmDeck *<# addCards =<< mapM runHook =<<
                                shuffle (filter (notIn cs) allDecks)
              where runHook c = runSetupHooks cs c >> return c
          a = do (self,h,price) <- getSHP
                 plusCoin 2
                 coins <- withTurn $ gets turnCoins
                 let treasure = sum $ map getTreasure h -- mapM...
                 let money = coins + sum (map getTreasure h)
                 cs <-3<* bmDeck
                 tell self $ "Current money: "++show money
                 let f = do
                       [c] <- askCards self (filter (\c->price c<=money) cs)
                              (OtherQuestion "black market buy") (0,1)
                       let cost = price c
                           needed = if cost>coins
                                    then " (need "++show (cost-coins)++" more)"
                                    else ""
                       ts <- askCards self (filter isTreasure h)
                             (OtherQuestion $ "play treasures"++needed)
                             (0,length h)
                       -- we might try to make these more atomic...?
                       plusCoin $ sum $ map getTreasure ts
                       played << ts
                       coins' <- withTurn $ gets turnCoins
                       if cost <= coins'
                          then do plusCoin (-cost)
                                  discard self *<< [c] -- only gain from supply
                                  return [c]
                          else return []
                 buy <- catchError f (\_ -> return [])
                 -- allow reordering of submerged cards?
                 bmDeck .<< filter (notIn buy) cs
                 

envoy :: Card
envoy = Card 0 4 "Envoy" "..." [action a]
    where a = try $ do
                self <- getSelf
                lho <- getLHO self
                cs <-5<* deck self
                [c] <- askCards lho cs (OtherQuestion "envoy discard") (1,1)
                discard self *<< [c]
                hand self << filter (/=c) cs

-- *Intrigue

baron :: Card
baron = Card 0 4 "Baron" "..." [action a]
    where a = do self <- getSelf
                 plusBuy 1
                 catchError `flip` (\_ -> gain self discard *<< [estate]) $ do
                   e:_ <- filter (sameName estate) `fmap` getStack (hand self)
                   True <- askYN self "Discard estate?"
                   discard self *<< [e]
                   plusCoin 4

bridge :: Card
bridge = Card 0 4 "Bridge" "..." [action a]
    where a = do plusABCD 0 1 1 0
                 withTurn $ modify $ \s -> s { turnPriceMod
                                                   = dec . turnPriceMod s }
          dec x | x<=0      = 0
                | otherwise = x-1

conspirator :: Card
conspirator = Card 0 4 "Conspirator" "..." [action a]
    where a = try $ do plusCoin 2
                       _:_:_ <- filter isAction `fmap` getStack played
                       plusABCD 1 0 0 1

courtyard :: Card
courtyard = Card 0 2 "Courtyard" "..." [action a]
    where a = do plusCard 3
                 (self,h,_) <- getSHP
                 deck self *<# askCards self h 
                               (UndrawBecause "courtyard") (1,1)

duke :: Card
duke = Card 0 5 "Duke" "..." [Victory, Score s]
    where s n = do self <- getSelf
                   c <- (length . filter (sameName duchy))
                        `fmap` allCards self
                   return $ n + c

greatHall :: Card
greatHall = Card 0 3 "Great Hall" "..." [Victory, Score $ return.(1+),action a]
    where a = plusABCD 1 0 0 1

harem :: Card
harem = Card 0 6 "Harem" "2 Treasure, 2VP" [Victory, Treasure 2,
                                            Score $ return.(2+)]

ironworks :: Card
ironworks = Card 0 4 "Ironworks" "..." [action a]
    where a = try $ do self <- getSelf
                       sup <- supplyCosting (<=4)
                       [c] <- askCards self sup SelectGain (1,1)
                       gain self discard *<< [c]
                       when (isAction c) $ plusAction 1
                       when (isTreasure c) $ plusCoin 1
                       when (isVictory c) $ plusCard 1

masquerade :: Card
masquerade = Card 0 3 "Masquerade" "..." [action a]
    where a = do plusCard 2
                 ps <- getAllPlayers
                 cs <- forM ps $ \p -> do h <- getStack $ hand p
                                          askCards p h
                                              (OtherQuestion "Pass left") (1,1)
                 ps' <- mapM getLHO ps
                 zipWithM_ (\p c -> hand p << c) ps' cs
                 (self,h,_) <- getSHP
                 trash <# askCards self h (TrashBecause "masquerade") (0,1)

miningVillage :: Card
miningVillage = Card 0 4 "Mining Village" "..." [Action a]
    where a this _ = do plusABCD 2 0 0 1
                        self <- getSelf
                        loc <- cardWhere this
                        when (loc /= stackName trash) $ do
                          tr <- askYN self "Trash mining village for +2 Coins?"
                          when tr $ do trash << [this]
                                       plusCoin 2

minion :: Card
minion = Card 0 5 "Minion" "..." [action a]
    where a = do self <- getSelf
                 go <- attack "minion"
                 plusAction 1
                 askMC self [("+2 Coins",plusCoin 2),
                             ("Discard hand, +4 Cards",att go)] "Choose one"
          att go = do self <- getSelf
                      discard self *<<< hand self
                      plusCard 4
                      go $ \_ opp -> do h <- getStack $ hand opp
                                        when (length h>4) $ do
                                          discard opp *<<< hand opp
                                          draw 4 opp

nobles :: Card
nobles = Card 0 6 "Nobles" "..." [Victory, Score $ return.(2+), action a]
    where a = do self <- getSelf
                 askMC self [("+3 Cards",plusCard 3),
                             ("+2 Actions",plusAction 2)] "Choose one"

pawn :: Card
pawn = Card 0 2 "Pawn" "..." [action $ getSelf >>= a]
    where a self = askMC2 self `flip` "Choose two" $
                   [("+1 Card",plusCard 1),("+1 Action",plusAction 1),
                    ("+1 Buy",plusBuy 1),("+1 Coin",plusCoin 1)]

saboteur :: Card
saboteur = Card 0 5 "Saboteur" "..." [action a]
    where a = attackNow "saboteur" $ \_ -> att []
          att cs opp = try $ do [c] <-1<* deck opp
                                p <- priceM c
                                if p<3 then att (c:cs) opp else do
                                trash << [c]
                                sup <- supplyCosting (<=(p-2))
                                gain opp discard *<#
                                     askCards opp sup SelectGain (1,1)

scout :: Card
scout = Card 0 4 "Scout" "..." [action a]
    where a = do self <- getSelf
                 plusAction 1
                 cs <-4<* deck self
                 let (v,nv) = partition isVictory cs
                     n = length nv
                 hand self << v
                 deck self *<# askCards self nv (UndrawBecause "scout") (n,n)

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

shantyTown :: Card
shantyTown = Card 0 3 "Shanty Town" "..." [action a]
    where a = do plusAction 2
                 as <- filter isAction `fmap` (getStack . hand =<< getSelf)
                 when (null as) $ plusCard 2 

steward :: Card
steward = Card 0 3 "Steward" "..." [action $ getSelf >>= a]
    where a self = askMC self `flip` "Choose one" $
                   [("+2 Cards",plusCard 2),
                    ("+2 Coins",plusCoin 2),
                    ("Trash 2 cards",tr)] -- maybe check if >=2 cards first?
          tr = do (self,h,_) <- getSHP
                  if length h<2 then a self else do
                  trash <# askCards self h (TrashBecause "steward") (2,2)

swindler :: Card
swindler = Card 0 3 "Swindler" "..." [action a]
    where a = do plusCoin 2
                 attackNow "saboteur" $ \self opp -> try $ do
                   [c] <-1<* deck opp
                   trash << [c]
                   p <- priceM c
                   sup <- supplyCosting (==p)
                   tell opp $ "Trashed "++show c
                   let q = "Trashed "++show c++": replacement?"
                   gain opp discard *<# -- not gain for purpose of smugglers...
                        askCards self sup (OtherQuestion q) (1,1)

torturer :: Card
torturer = Card 0 5 "Torturer" "..." [action a]
    where a = do plusCard 3 -- this attack may not be parallelizable
                 attackNow "torturer" $ \_ opp -> try $ do
                   let curs = gain opp hand << [curse]
                       disc = forceDiscard "torturer" opp 2
                   h <- getStack $ hand opp
                   c <- length `fmap` supplyCards (cardName curse)
                   case () of
                     _ | length h<2 -> curs
                       | c==0       -> disc
                   askMC opp [("Discard 2 cards",disc),
                              ("Gain a Curse into hand",curs)] "Choose one"

tradingPost :: Card
tradingPost = Card 0 5 "Trading Post" "..." [action a]
    where a = do (self,h,_) <- getSHP
                 cs <- askCards self h (TrashBecause "trading post") (2,2)
                 trash << cs
                 when (length cs == 2) $ try $ gain self hand << [silver]

tribute :: Card
tribute = Card 0 5 "Tribute" "..." [action a]
    where a = do lho <- getSelf >>= getLHO
                 cs <-2<* deck lho
                 discard lho *<< cs
                 let cs' = nubBy sameName cs
                 forM_ cs' $ \c -> do
                   when (isTreasure c) $ plusCoin 2
                   when (isAction c)   $ plusAction 2
                   when (isVictory c)  $ plusCard 2

upgrade :: Card
upgrade = Card 0 5 "Upgrade" "..." [action $ try a]
    where a = do (self,h,price) <- getSHP
                 plusABCD 1 0 0 1
                 [c] <- askCards self h (TrashBecause "upgrade") (1,1)
                 sup <- supplyCosting (==(price c+1))
                 gain self discard *<# askCards self sup SelectGain (1,1)

wishingWell :: Card
wishingWell = Card 0 3 "Wishing Well" "..." [action $ try a]
    where a = do plusABCD 1 0 0 1
                 self <- getSelf
                 [t] <-1<* deck self
                 cs <- nubBy sameName `fmap` allCards self
                 [g] <- askCards self cs (OtherQuestion "wishing well") (1,1)
                 when (sameName t g) $ hand self << [t]

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
                 if move then deck self *<< [c] else deck self .<< [c]

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

-- smugglers - needs to hook into gain... :-/ (and check if gained during turn)

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
isVictory c = not $ null [() | Victory <- cardType c]

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

sameName :: Card -> Card -> Bool
sameName = (==) `on` cardName

-- Want some sort of asynchrony, in that we can go on with our
-- turn while others are selecting their reactions...
-- BUT, we need to also be able to wait for their reactions
-- and for the attack resolutions before going on if we
-- want to.

