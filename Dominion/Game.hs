module Dominion.Game ( start, play ) where

import Dominion.Types
import Dominion.Cards
import Dominion.Question
import Dominion.Stack

import Control.Concurrent ( forkIO )
import TCP.Chan ( Output, Input, pipe, readInput, writeOutput )
import Control.Monad.State ( execStateT, modify, gets, liftIO )
import Control.Monad ( replicateM, foldM, when, forever )

newTurn :: TurnState
newTurn = TurnState 1 1 0 cardPrice []

start :: [(String,Output MessageToClient)] -> Input (QId,[Answer])
      -> [Card] -> IO GameState
-- we should actually work in the Game monad for a bit here....
start ps c cs = do (chi,cho) <- pipe
                   forkIO $ forever $ readInput c >>=
                         writeOutput cho . uncurry AnswerFromClient
                   forkIO $ respond chi []
                   execStateT `flip` emptyState chi cho $ do
                     mapM_ fillDeck allPlayers
                     mapM_ (draw 5) allPlayers
                     fillSupplyN (10*(length ps-1)) curse
                     fillSupplyN provs province
                     mapM_ fillSupply [duchy,estate]
                     mapM_ (fillSupplyN 30) [gold,silver]
                     fillSupplyN 40 copper
                     mapM_ fillSupply $ reverse cs
    where allPlayers = map PId [0..length ps-1]
          fillDeck p = do es <- replicateM 3 (copyCard estate)
                          cs <- replicateM 7 (copyCard copper)
                          discard p *<<@ (es++cs)
          emptyPlayer (i,(s,c)) = PlayerState i s c [] [] [] [] [] []
          emptyState chi cho =
              GameState (map emptyPlayer $ zip [0..] ps) []
                            0 newTurn defaultGain chi cho [0..] [0..]
          fillSupplyN n c' = do card <- copyCard c'
                                modify $ \s -> s { gameSupply =
                                                    (card,n):gameSupply s }
          vic = if length ps<3 then 8 else 12
          provs = if length ps<=4 then vic else 3*(length ps)
          fillSupply c' = fillSupplyN (if isVictory c' then vic else 10) c'
          respond ch rs = do r <- readInput ch
                             case r of
                               AnswerFromClient q as -> do
                                  u <- maybe (return False) ($as) $ lookup q rs
                                  let rs' = if u then remove q rs else rs
                                  respond ch rs'
                               RegisterQuestion q f -> respond ch $ (q,f):rs
          remove q [] = []
          remove q ((q',f):xs) | q==q' = xs
          remove q (x:xs) = x:remove q xs


play :: Game [(String,Int)]
play = do winner <- endGame
          case winner of
            Just s -> return s
            Nothing -> turn >> play
    where endGame = do sup <- gets gameSupply
                       let empty = filter ((<=0).snd) sup
                           prov = map snd $ filter (isProvince.fst) sup
                           over = length empty >= 3 || head prov <= 0
                       if not over then return Nothing else do
                       ps <- gets gamePlayers
                       (Just . zip (map playerName ps))
                           `fmap` mapM playerScore ps
          playerScore :: PlayerState -> Game Int
          playerScore p = foldM (flip ($)) 0 $
                          concatMap (getScores . cardType) $ allCards p
          isProvince = (=="Province") . cardName
          allCards p = playerHand p ++ playerDeck p ++ playerDiscard p
                       ++ playerDuration p ++ concatMap snd (playerMats p)
          getScores [] = []
          getScores (Score f:xs) = f:getScores xs
          getScores (_:xs) = getScores xs
                           
turn :: Game ()
turn = do self <- gets currentTurn
          modify $ \s -> s { turnState = newTurn }
          duration self
          actions self
          coins <- gets $ turnCoins . turnState
          treasure <- (sum . map getTreasure) `fmap` hand self
          buys <- gets $ turnBuys . turnState
          gain <- gets hookGain
          supply <- gets gameSupply
          tell self $ "Supply: " ++ show supply
          buy self buys (coins + treasure) gain
          cleanup self
          -- next turn
          n <- gets $ length . gamePlayers
          modify $ \s -> s { currentTurn = (self+PId 1)`mod`(PId n) }
    where actions self = do h <- hand self
                            a <- withTurn $ gets turnActions
                            tell self $ "Actions ("++show a++"): hand="++show h
                            let as = filter (isAction) h
                            cs <- askCards self as SelectAction (0,1)
                            when (not $ null cs) $ do
                              plusAction (-1)
                              played *<<& (cs,hand self)
                              getAction $ head cs
                              acts <- gets $ turnActions . turnState
                              when (acts > 0) $ actions self
          buy self buys money gain
              = do -- supply <- gets gameSupply
                   tell self $ "Buy: " ++ show money ++ " coins, "
                               ++ show buys ++ " buys"
                   -- tell self $ "  supply=" ++ show supply
                   price <- gets $ turnPriceMod . turnState
                   sup <- supplyCosting (<=money)
                   cs <- askCards self sup SelectBuy (0,1)
                   case cs of
                     [c] -> do gain self c
                               when (buys>1) $
                                    buy self (buys-1) (money - price c) gain
                     _ -> return ()
          duration self = do played *<<< durations self
                             withPlayer self (gets durationEffects)
                                            >>= sequence_
                             withPlayer self (modify $
                                            \s -> s { durationEffects = [] })
          cleanup self = do discard self *<<< played
                            discard self *<<< hand self
                            draw 5 self
