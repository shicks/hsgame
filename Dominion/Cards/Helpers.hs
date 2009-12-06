module Dominion.Cards.Helpers where

import Dominion.Types
import Dominion.Stack
import Dominion.Question

import Control.Monad.State ( gets, modify )
import Control.Monad ( when )
import Data.Maybe ( maybeToList )
import Data.List ( nubBy )

getSHP :: Game (PId, [Card], Card -> Int) 
getSHP = do self <- gets currentTurn
            h <- getStack $ hand self
            price <- withTurn $ gets priceMod
            return (self,h,price)

-- helper functions
affords :: Game (Int -> Card -> Bool)
affords = do price <- withTurn $ gets priceMod
             return $ \p c -> price c <= p 

priceIsM :: (Int->Bool) -> Card -> Game Bool
priceIsM f c = do p <- priceM c
                  return $ f p

priceM :: Card -> Game Int
priceM c = do withTurn $ ($c) `fmap` gets priceMod

distinctSupplies :: Game [Card]
distinctSupplies = nubBy sameName `fmap` allSupply

supplyCosting :: (Int -> Bool) -> Game [Card]
supplyCosting f = do price <- withTurn $ gets priceMod
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

forceUndraw :: String -> PId -> Int -> Game ()
forceUndraw s p n = do h <- getStack $ hand p
                       deck p *<# askCards p h (UndrawBecause s) (n,n)

-- deal with throne room later...!
nextTurn :: Game () -> Game ()
nextTurn later = do self <- getSelf
                    withPlayer self $ modify $
                                   \s -> s { durationEffects =
                                             later:durationEffects s }

action :: Game () -> CardType
action act = Action $ \_ _ -> act

duration :: Game () -> CardType
duration act = Action $ \this pre -> do self <- getSelf
                                        let save = this:maybeToList pre
                                        durations self << save
                                        act

oneShot :: Game () -> CardType
oneShot act = Action $ \this _ -> do trash << [this]
                                     act


isVictory :: Card -> Bool
isVictory c = not $ null [() | Victory <- cardType c]

isTreasure :: Card -> Bool
isTreasure c = not $ null [() | Treasure _ <- cardType c]

getTreasure :: Card -> Game Int
getTreasure c = withTurn $ ($c) `fmap` gets treasureMod

faceValue :: Card -> Int
faceValue c = sum [t | Treasure t <- cardType c] 

isAction :: Card -> Bool
isAction c = not $ null [() | Action _ <- cardType c]

getAction :: Card -> Game ()
getAction c = mapM_ id [a c Nothing | Action a <- cardType c]

getActionPred :: Card -> Card -> Game ()
getActionPred pre c = foldl (>>) (return ())
                      [a c (Just pre) | Action a <- cardType c]

finally :: Game () -> Game () -> Game ()
finally job after = try job >> after

-- Want some sort of asynchrony, in that we can go on with our
-- turn while others are selecting their reactions...
-- BUT, we need to also be able to wait for their reactions
-- and for the attack resolutions before going on if we
-- want to.

