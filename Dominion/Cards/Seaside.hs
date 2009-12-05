module Dominion.Cards.Seaside where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question
import Dominion.Message
import Dominion.Cards.Helpers
import Dominion.Cards.Base
import Dominion.Cards.Intrigue

import Control.Monad.State ( gets, modify, liftIO )
import Control.Monad ( (<=<), when, unless, forM_, filterM )
import Data.Maybe ( maybeToList, fromMaybe )
import Data.List ( (\\), partition, intercalate )

import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )
import System.IO.Unsafe ( unsafePerformIO )

seaside :: [Card]  -- outpost, treasury, smugglers, .....
seaside = [ambassador, bazaar, caravan, cutpurse, embargo, explorer,
           fishingVillage, ghostShip, haven, island, lighthouse,
           lookout, merchantShip, nativeVillage, navigator, outpost,
           pearlDiver, pirateShip, salvager, seaHag, smugglers,
           tactician, treasureMap, treasury, warehouse, wharf]

seasideSets :: [(String,[Card])]
seasideSets =
    [("High Seas",
      [embargo, explorer, haven, island, pirateShip,
       smugglers, bazaar, caravan, lookout, wharf ]),
     ("Buried Treasure",
      [ambassador, cutpurse, lighthouse, outpost, treasureMap,
       fishingVillage, pearlDiver, tactician, warehouse, wharf]),
     ("Shipwrecks",
      [ghostShip, nativeVillage, navigator, seaHag, smugglers,
       treasury, merchantShip, pearlDiver, salvager, warehouse]),
     ("Reach for Tomorrow",
      [ghostShip, lookout, seaHag, treasureMap, cutpurse,
       adventurer, cellar, councilRoom, spy, village]),
     ("Repetition",
      [explorer, pirateShip, treasury, outpost, caravan,
       chancellor, festival, militia, pearlDiver, workshop]),
     ("Give and Take",
      [haven, island, salvager, ambassador, smugglers,
       fishingVillage, library, market, moneylender, witch])]

-- Seaside cards

ambassador :: Card
ambassador = Card 0 3 "Ambassador" "..." [action $ try a]
    where a = do att <- attack "ambassador"
                 (self,h,_) <- getSHP
                 [c] <- askCards self h (GiveAway "ambassador") (1,1)
                 let n = length $ filter (sameName c) h
                     opts2 = if n>1 then [("2",return 2)] else []
                     opts = ("0",return 0):("1",return 1):opts2
                 name <- withPlayer self $ gets playerName
                 num <- askMC self opts "Return how many?"
                 tellAll $ InfoMessage $ name++" ungained "++show num
                                         ++" copies of "++cardName c
                 supply << take num (filter (sameName c) h)
                 att $ \_ opp -> gain opp discard *<< [c]

bazaar :: Card
bazaar = Card 0 5 "Bazaar" "..." [action $ plusABCD 2 0 1 1]

caravan :: Card
caravan = Card 0 4 "Caravan" "..." [duration a]
    where a = plusABCD 1 0 0 1 >> nextTurn (plusCard 1)

cutpurse :: Card
cutpurse = Card 0 4 "Cutpurse" "..." [action a]
    where a = do plusCoin 2
                 attackNow "cutpurse" $ \_ opp -> do
                   h <- getStack $ hand opp
                   let cu = filter (sameName copper) h
                   if null cu then revealHand opp
                              else discard opp *<< take 1 cu

embargo :: Card  -- can we hook into the the card's show for supply?!?
embargo = Card 0 2 "Embargo" "..." [oneShot $ try a]
    where a = do self <- getSelf
                 plusCoin 2
                 sup <- supplyCosting $ \_ -> True -- all piles
                 [c] <- askCards self sup (OtherQuestion "embargo") (1,1)
                 modify $ \s -> s { hookBuy = hook c:hookBuy s }
          hook e p cs = forM_ cs $ \c -> when (sameName c e) $
                                         gain p discard *<< [curse]

explorer :: Card
explorer = Card 0 5 "Explorer" "..." [action a]
    where a = do (self,h,_) <- getSHP
                 let provs = filter (sameName province) h
                 if null provs
                    then gain self hand << [silver]
                    else askMC self `flip` "Reveal a province?" $
                         [("Yes",do revealCards self (take 1 provs) "hand"
                                    gain self hand << [gold]),
                          ("No",gain self hand << [silver])]

fishingVillage :: Card
fishingVillage = Card 0 3 "Fishing Village" "..." [duration a]
    where a = plusABCD 2 0 1 0 >> nextTurn (plusABCD 1 0 1 0)

ghostShip :: Card
ghostShip = Card 0 5 "Ghost Ship" "..." [action a]
    where a = do plusCard 2
                 attackNow "ghost ship" $ \_ opp -> do
                   h <- getStack $ hand opp
                   let n = length h
                   when (n>3) $ forceUndraw "ghost ship" opp (n-3)
                     
                 

haven :: Card
haven = Card 0 2 "Haven" "..." [duration $ try $ getSelf >>= a]
    where a self = do plusABCD 1 0 0 1
                      [c] <- askCardsHand (OtherQuestion "haven") (1,1)
                      durations self << [c]
                      nextTurn $ hand self << [c]

island :: Card
island = Card 0 4 "Island" "..." [Action a, Victory, Score $ return.(1+)]
    where a this _ = try $ do self <- getSelf
                              iMat self *<< [this]
                              iMat self *<# askCardsHand
                                       (DiscardBecause "island") (1,1)
          iMat = mat "island"

lighthouse :: Card
lighthouse = Card 0 2 "Lighthouse" "..." [duration a,DReaction r]
    where a = plusABCD 1 0 1 0 >> nextTurn (plusCoin 1) 
          r _ _ = return $ \_ _ _ -> return ()

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

nativeVillage :: Card
nativeVillage = Card 0 2 "Native Village" "..." [action a]
    where a = do self <- getSelf
                 plusAction 2
                 cs <- map cardName `fmap` getStack (nvMat self)
                 tellSelf $ InfoMessage $ "Native Village Mat: "++
                          intercalate ", " cs
                 askMC self `flip` "Choose one" $
                           [("Set aside",setAside self),
                            ("Draw from mat",hand self <<< nvMat self)]
          setAside self = do nvMat self *<#1<* deck self
                             cs <- map cardName `fmap` getStack (nvMat self)
                             tellSelf $ InfoMessage $ "Native Village Mat: "++
                                      intercalate ", " cs
          nvMat = mat "nativeVillage"

navigator :: Card
navigator = Card 0 4 "Navigator" "..." [action a]
    where a = do self <- getSelf
                 plusCoin 2
                 cs <-5<* deck self
                 askMC self [("Discard all",discard self *<< cs),
                             ("Rearrange",rearr self cs)] $
                     "Drew "++intercalate ", " (map cardName cs)
          rearr self cs = deck self *<#
                          askCards self cs (OtherQuestion
                                            "Pick order (bottom first)") (5,5)

outpost :: Card
outpost = Card 0 5 "Outpost" "..." [duration a]
    where a = do self <- getSelf
                 cur <- getStack prevDuration
                 unless (any (sameName outpost) cur) $
                   withTurn $ modify $ \s -> s { nextTurnHook = \_->plusCard 3 }

pearlDiver :: Card
pearlDiver = Card 0 2 "Pearl Diver" "..." [action $ try a]
    where a = do self <- getSelf
                 plusAction 1 >> plusCard 1
                 [c] <-1<. deck self
                 move <- askYN self $ "Move " ++ show c ++ " to top?"
                 if move then deck self *<< [c] else deck self .<< [c]

{-# NOINLINE pirateData #-}
pirateData :: IORef [(PId,Int)]
pirateData = unsafePerformIO $ newIORef []

pirateShip :: Card
pirateShip = Card 0 4 "Pirate Ship" "..." [action $ getSelf >>= a]
    where a self = do att <- attack "pirate ship"
                      dat <- liftIO $ readIORef pirateData
                      let loot = fromMaybe 0 $ lookup self dat
                      askMC self `flip` "Choose one" $
                            [("+"++show loot++" Coins",plusCoin loot),
                             ("Loot opponents",go att)]
          go att = do succ <- liftIO $ newIORef False
                      att $ \self opp -> try $ do
                        cs <-2<* deck opp
                        let (ts,nts) = partition isTreasure cs
                        discard opp *<< nts
                        [c] <- askCards self ts (TrashBecause "pirate") (1,1)
                        discard opp *<< filter (/=c) ts
                        liftIO $ writeIORef succ True
                      try $ do True <- liftIO $ readIORef succ
                               self <- getSelf
                               liftIO $ modifyIORef pirateData $ inc self
          inc p [] = [(p,1)]
          inc p ((p',x):xs) | p==p' = (p',1+x):xs
                            | otherwise = (p',x):inc p xs

salvager :: Card
salvager = Card 0 4 "Salvager" "..." [action a]
    where a = do plusBuy 1
                 trash <# mapM_ (plusCoin <=< priceM) #<#
                     askCardsHand (TrashBecause "salvager") (1,1)

seaHag :: Card
seaHag = Card 0 4 "Sea Hag" "..." [action a]
    where a = attackNow "sea hag" $ \_ opp -> do
                discard opp *<#1<* deck opp
                gain opp deck *<< [curse]

{-# NOINLINE smugglerData #-}
smugglerData :: IORef [(PId,[Card])]
smugglerData = unsafePerformIO $ newIORef []

-- http://www.boardgamegeek.com/thread/455785/page/2
-- we add all gains, since provinces can cost 6 later
smugglers :: Card
smugglers = Card 0 3 "Action" "..." [Hook (SetupHook setup),action a]
    where setup _ = modify $ \s -> s { hookGain = gainHook:hookGain s,
                                       hookTurn = turnHook:hookTurn s }
          turnHook :: PId -> Game ()
          turnHook p = liftIO $ modifyIORef smugglerData $ clear p
          gainHook :: PId -> [Card] -> Game ()
          gainHook p cs = liftIO $ modifyIORef smugglerData $ add p cs
          clear p [] = []
          clear p ((p',cs):xs) | p==p' = xs
                               | otherwise = (p',cs):clear p xs
          add p c [] = [(p,c)]
          add p c ((p',cs):xs) | p==p' = (p',cs++c):xs
                               | otherwise = (p',cs):add p c xs
          a = do self <- getSelf
                 rho <- getRHO self
                 dat <- liftIO (readIORef smugglerData)
                 let rhos = concat $ maybeToList $ lookup rho dat
                 avail <- filterM inSupply =<< filterM (priceIsM (<=6)) rhos
                 gain self discard *<# askCards self avail SelectGain (1,1)

tactician :: Card
tactician = Card 0 5 "Tactician" "..." [duration a]
    where a = do (self,h,_) <- getSHP
                 unless (null h) $ nextTurn $ plusABCD 1 1 0 5
                 discard self *<<< hand self

treasureMap :: Card
treasureMap = Card 0 4 "Treasure Map" "..." [Action a]
    where a this _ = do (self,h,_) <- getSHP
                        let maps = filter (sameName treasureMap) h
                        cs <- askCards self maps (TrashBecause
                                                  "treasure map") (1,1)
                        t <- inTrash this
                        when (not (null cs) && not t) $
                             gain self deck *<< replicate 4 gold
                        trash << this:cs -- everything gets trashed

{-# NOINLINE treasuryData #-}
treasuryData :: IORef Bool
treasuryData = unsafePerformIO $ newIORef True -- can we recycle?

treasury :: Card
treasury = Card 0 5 "Treasury" "..." [Hook (SetupHook setup),Action a]
    where setup _ = modify $ \s -> s { hookBuy  = buyHook :hookBuy s,
                                       hookTurn = turnHook:hookTurn s }
          buyHook _ = mapM_ $ \c -> when (isVictory c) $ liftIO $
                                     writeIORef treasuryData False
          turnHook _ = liftIO $ writeIORef treasuryData True -- reset each turn
          a this _ = do plusABCD 1 0 1 1
                        withTurn $ modify $ \s -> s { cleanupHooks =
                                          recycle this:cleanupHooks s }
          recycle this = do dat <- liftIO $ readIORef treasuryData
                            when dat $ do
                              self <- getSelf
                              askMC self [("Yes",deck self *<< [this]),
                                          ("No",return ())]
                                        "Put Treasury on top of deck?"

warehouse :: Card
warehouse = Card 0 3 "Warehouse" "..." [action a]
    where a = do self <- getSelf
                 plusAction 1 >> draw 3 self
                 discard self *<#
                         askCardsHand (DiscardBecause "warehouse") (3,3)

wharf :: Card
wharf = Card 0 5 "Wharf" "..." [duration a]
    where a = plusABCD 0 1 0 2 >> nextTurn (plusABCD 0 1 0 2)
