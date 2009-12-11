module Dominion.Cards.Base where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question
import Dominion.Message
import Dominion.Cards.Helpers

import Control.Monad.State ( gets )
import Control.Monad.Error ( catchError )
import Control.Monad ( when )
import Data.Maybe ( fromMaybe )
import Data.List ( partition )

dominion :: [Card]
dominion = [adventurer, bureaucrat, cellar, chancellor, chapel,
            councilRoom, feast, festival, gardens, laboratory,
            library, market, militia, mine, moat, moneylender,
            remodel, smithy, spy, thief, throneRoom, village,
            witch, woodcutter, workshop]

dominionSets :: [(String, [Card])]
dominionSets =
    [("First Game",
      [cellar, market, militia, mine, moat, remodel, smithy, village,
       woodcutter, workshop]),
     ("Big Money",
      [adventurer, bureaucrat, chancellor, chapel, feast, laboratory,
       market, mine, moneylender, throneRoom]),
     ("Interaction",
      [bureaucrat, chancellor, councilRoom, festival, library, militia,
       moat, spy, thief, village]),
     ("Size Distortion",
      [cellar, chapel, feast, gardens, laboratory, thief, village, witch,
       woodcutter, workshop]),
     ("Village Square",
      [bureaucrat, cellar, festival, library, market, remodel, smithy,
       throneRoom, village, woodcutter])]

-- base set
adventurer :: Card
adventurer = Card 0 6 "Adventurer" "..." [action $ getSelf >>= a ]
    where a self = finally (dig 0) $ discard self *<<< aside
          dig 2 = return ()
          dig n = do self <- getSelf
                     [c] <-1<* deck self
                     revealCards self [c] "deck"
                     if isTreasure c
                        then do hand self << [c]
                                dig (n+1)
                        else do aside << [c]
                                dig n

bureaucrat :: Card
bureaucrat = Card 0 4 "Bureaucrat" "..." [action a]
    where a = do attackNow "bureaucrat" $ \_ opp -> try $ do
                   h <- getStack $ hand opp
                   let vs = filter isVictory h
                   if null vs then revealHand opp else do -- reveal hand...?
                   [c] <- askCards opp vs (UndrawBecause "bureaucrat") (1,1)
                   revealCards opp [c] "hand"
                   deck opp *<< [c]
                 self <- getSelf
                 try $ gainFromSupply deck self silver 1

cellar :: Card
cellar = Card 0 2 "Cellar" "..." [action a]
    where a = do self <- getSelf
                 plusAction 1
                 cs <- askCardsHand (DiscardBecause "cellar") (0,-1)
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
    where a = trash <# askCardsHand (TrashBecause "chapel") (0,4)

councilRoom :: Card
councilRoom = Card 0 5 "Council Room" "..." [action a]
    where a = do plusCard 4
                 getSelf >>= getOpponents >>= mapM_ (draw 1)

feast :: Card
feast = Card 0 4 "Feast" "Trash this card.  Gain a card costing up to 5"
        [oneShot a]
    where a = do self <- getSelf
                 sup <- supplyCosting (<=5)
                 c <- askCards self sup SelectGain (1,1)
                 gainCardsTop discard self c

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
                 cnew <- askCards self sup SelectGain (1,1)
                 let ohand = (`SPId` "hand") -- FIXME!
                 gainCardsTop ohand self cnew

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
    where a = do self <- getSelf
                 [c] <- askCardsHand (TrashBecause "remodel") (1,1)
                 trash << [c]
                 sup <- supplyCosting . (>=) . (2+) =<< priceM c
                 newc <- askCards self sup SelectGain (1,1)
                 gainCardsTop discard self newc

smithy :: Card
smithy = Card 0 4 "Smithy" "..." [action $ plusCard 3]

spy :: Card
spy = Card 0 4 "Spy" "..." [action a]
    where a = do self <- getSelf
                 plusAction 1 >> plusCard 1
                 dospy (\_ -> "Self") self self
                 attackNow "spy" $ dospy ("Opponent "++)
          dospy fmt self p =
              try $ do [c] <-1<* deck p
                       who <- withPlayer p $ fmt `fmap` gets playerName
                       let msg = "("++who++"): Discard "++show c++"?"
                           choices = [("Discard",discard p *<< [c]),
                                      ("Keep",return ())]
                       askMC self choices msg

thief :: Card
thief = Card 0 4 "Thief" "..." [action a]
    where a = attackNow "thief" $ \self opp -> try $ do
                cs <-2<* deck opp
                revealCards opp cs "deck"
                let (ts,nts) = partition isTreasure cs
                discard opp *<< nts
                [c] <- askCards self ts (TrashBecause "thief") (1,1)
                discard opp *<< filter (/=c) ts
                askMC self [("Steal",gainCardsTop discard self [c]),
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
    where a this pre = try $ do (self,h,_) <- getSHP
                                let as = filter isAction h
                                [c] <- askCards self as SelectAction (1,1)
                                played << [c]
                                getAction c
                                getActionPred (fromMaybe this pre) c

village :: Card
village = Card 0 3 "Village" "..." $ [action $ plusABCD 2 0 0 1]

witch :: Card
witch = Card 0 5 "Witch" "..." [action a]
    where a = do plusCard 2
                 attackNow "witch" $ \_ o ->
                     try $ gainFromSupply discard o curse 1

woodcutter :: Card
woodcutter = Card 0 3 "Woodcutter" "..." [action $ plusABCD 0 1 2 0]

workshop :: Card
workshop = Card 0 3 "Workshop" "..." [action a]
    where a = do self <- getSelf
                 sup <- supplyCosting (<=4)
                 c <- askCards self sup SelectGain (1,1)
                 gainCardsTop discard self c

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

