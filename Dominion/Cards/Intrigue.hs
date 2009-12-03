module Dominion.Cards.Intrigue where

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question
import Dominion.Message
import Dominion.Cards.Helpers
import Dominion.Cards.Base

import Control.Monad.State ( modify )
import Control.Monad.Error ( catchError )
import Control.Monad ( when, forM, forM_, zipWithM_ )
import Data.List ( nubBy, partition )

intrigue :: [Card]
intrigue = [baron, bridge, conspirator, coppersmith, courtyard,
            duke, greatHall, harem, ironworks, masquerade,
            miningVillage, minion, nobles, pawn, saboteur,
            scout, secretChamber, shantyTown, steward,
            swindler, torturer, tradingPost, tribute,
            upgrade, wishingWell]

intrigueSets :: [(String, [Card])]
intrigueSets =
    [("Underlings",
     [baron, masquerade, minion, nobles, pawn, steward,
      cellar, festival, library, witch]),
     ("Hand Madness",
      [minion, nobles, steward, torturer, bureaucrat,
       chancellor, councilRoom, courtyard, mine, militia ]),
     ("Deconstruction",
      [bridge, miningVillage, saboteur, swindler, torturer,
       remodel, secretChamber, spy, thief, throneRoom ]),
     ("Victory Dance",
      [bridge, ironworks, masquerade, nobles, pawn, scout, upgrade, duke,
       greatHall, harem ]),
     ("Secret Schemes",
      [conspirator, ironworks, pawn, saboteur, shantyTown, steward,
       swindler, tradingPost, upgrade, wishingWell, harem ]),
     ("Best Wishes",
      [coppersmith, masquerade, scout, shantyTown, steward, torturer,
       tradingPost, upgrade, wishingWell, courtyard ])
    ]

-- *Intrigue

baron :: Card
baron = Card 0 4 "Baron" "..." [action a]
    where a = do self <- getSelf
                 plusBuy 1
                 catchError `flip` (\_ -> gainEstate self) $ do
                   e:_ <- filter (sameName estate) `fmap` getStack (hand self)
                   True <- askYN self "Discard estate?"
                   discard self *<< [e]
                   plusCoin 4
          gainEstate self = try $ gain self discard *<< [estate]

bridge :: Card
bridge = Card 0 4 "Bridge" "..." [action a]
    where a = do plusABCD 0 1 1 0
                 withTurn $ modify $ \s -> s { priceMod = dec . priceMod s }
          dec x | x<=0      = 0
                | otherwise = x-1

conspirator :: Card
conspirator = Card 0 4 "Conspirator" "..." [action a]
    where a = try $ do plusCoin 2
                       _:_:_ <- filter isAction `fmap` getStack played
                       plusABCD 1 0 0 1

coppersmith :: Card
coppersmith = Card 0 4 "Coppersmith" "..." [action a]
    where a = withTurn $ modify $ \s -> s { treasureMod = f $ treasureMod s }
          f old c | sameName c copper = 1 + old c
                  | otherwise         = old c

courtyard :: Card
courtyard = Card 0 2 "Courtyard" "..." [action a]
    where a = do plusCard 3
                 self <- getSelf
                 deck self *<# askCardsHand (UndrawBecause "courtyard") (1,1)

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
                                              (GiveAway "Pass left") (1,1)
                 ps' <- mapM getLHO ps
                 zipWithM_ (\p c -> hand p << c) ps' cs
                 trash <# askCardsHand (TrashBecause "masquerade") (0,1)

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
    where a = attackNow "saboteur" att
          att self opp = finally (do [c] <-1<* deck opp
                                     aside << [c]
                                     p <- priceM c
                                     if p<3 then att self opp else do
                                        trash << [c]
                                        tell self $ "Trashed "++show c
                                        sup <- supplyCosting (<=(p-2))
                                        gain opp discard *<#
                                             askCards opp sup (q c) (1,1))
                         $ discard opp *<<< aside
          q c = GiveAway $ "Trashed "++show c++": replacement?"

scout :: Card
scout = Card 0 4 "Scout" "..." [action a]
    where a = do self <- getSelf
                 plusAction 1
                 cs <-4<* deck self
                 revealCards self cs "deck"
                 let (v,nv) = partition isVictory cs
                     n = length nv
                 hand self << v
                 deck self *<# askCards self nv (UndrawBecause "scout") (n,n)

secretChamber :: Card
secretChamber = Card 0 2 "Secret Chamber" "..." [action act,Reaction react]
    where react self cont = do draw 2 self
                               cs <- askCardsHand
                                     (UndrawBecause "secret chamber") (2,2)
                               deck self *<< cs
                               cont
          act = do self <- getSelf
                   cs <- askCardsHand (DiscardBecause "secret chamber") (0,-1)
                   discard self *<< cs
                   plusCoin $ length cs

shantyTown :: Card
shantyTown = Card 0 3 "Shanty Town" "..." [action a]
    where a = do self <- getSelf
                 plusAction 2
                 revealHand self
                 as <- filter isAction `fmap` (getStack $ hand self)
                 when (null as) $ plusCard 2 

steward :: Card
steward = Card 0 3 "Steward" "..." [action $ getSelf >>= a]
    where a self = askMC self `flip` "Choose one" $
                   [("+2 Cards",plusCard 2),
                    ("+2 Coins",plusCoin 2),
                    ("Trash 2 cards",tr)] -- maybe check if >=2 cards first?
          tr = trash <# askCardsHand (TrashBecause "steward") (2,2)

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
                        askCards self sup (GiveAway q) (1,1)

torturer :: Card
torturer = Card 0 5 "Torturer" "..." [action a]
    where a = do plusCard 3 -- this attack may not be parallelizable
                 attackNow "torturer" $ \_ opp -> try $ do
                   let curs = try $ gain opp hand << [curse]
                       disc = forceDiscard "torturer" opp 2
                   askMC opp [("Discard 2 cards",disc),
                              ("Gain a Curse into hand",curs)]
                             "Choose one"

tradingPost :: Card
tradingPost = Card 0 5 "Trading Post" "..." [action a]
    where a = do self <- getSelf
                 cs <- askCardsHand (TrashBecause "trading post") (2,2)
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
    where a = do (self,_,price) <- getSHP
                 plusABCD 1 0 0 1
                 [c] <- askCardsHand (TrashBecause "upgrade") (1,1)
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
