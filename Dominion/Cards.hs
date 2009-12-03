module Dominion.Cards ( module Dominion.Cards.Base,
                        module Dominion.Cards.Intrigue,
                        module Dominion.Cards.Seaside,
                        module Dominion.Cards.Helpers,
                        promos, blackMarket, envoy,
                        allDecks, allRecommended ) where

import Dominion.Cards.Base
import Dominion.Cards.Intrigue
import Dominion.Cards.Seaside
import Dominion.Cards.Helpers

import Dominion.Types
import Dominion.Stack
import Dominion.Attack
import Dominion.Question
import Dominion.Message

import Control.Monad.State ( gets )
import Control.Monad.Error ( catchError )

allDecks :: [Card]
allDecks = dominion ++ promos ++ intrigue ++ seaside

allRecommended :: [(String,[Card])]
allRecommended = dominionSets ++ intrigueSets ++ seasideSets

-- *Promo cards

promos :: [Card]
promos = [blackMarket, envoy]

-- It seems like we need access to all the decks somehow, so these are
-- going out here.  We could also pass in all the possible decks to the
-- setup hook, but that seems sillier.
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
                 treasure <- sum `fmap` mapM getTreasure h
                 let money = coins+treasure
                 cs <-3<* bmDeck
                 tell self $ "Current money: "++show money
                 let f = do
                       [c] <- askCards self (filter (\c->price c<=money) cs)
                              (Gain "black market buy") (0,1)
                       let cost = price c
                           needed = if cost>coins
                                    then " (need "++show (cost-coins)++" more)"
                                    else ""
                       ts <- askCards self (filter isTreasure h)
                             (OtherQuestion $ "play treasures"++needed)
                             (0,length h)
                       -- we might try to make these more atomic...?
                       plusCoin =<< sum `fmap` mapM getTreasure ts
                       played << ts
                       coins' <- withTurn $ gets turnCoins
                       if cost <= coins'
                          then do plusCoin (-cost)
                                  runBuyHooks self [c]
                                  gain' self discard *<< [c]
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
                revealCards self cs "deck"
                [c] <- askCards lho cs (Gain "envoy discard") (1,1)
                discard self *<< [c]
                hand self << filter (/=c) cs


