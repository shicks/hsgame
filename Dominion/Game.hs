module Game where

import Dominion.Types
import Dominion.Cards
import Dominion.Question
import Dominion.Stack

newTurn :: TurnState
newTurn = TurnState 1 1 0 cardPrice

start :: [(String,Chan MessageToClient)] -> Chan MessageToServer
      -> [Card] -> IO GameState
-- we should actually work in the Game monad for a bit here....
start ps c cs = execStateT `flip` emptyState $ do
                  mapM_ fillDeck [0..length ps-1]
                  mapM_ draw 5 [0..length ps-1]
    where fillDeck p = do es <- replicateM 3 (copyCard estate)
                          cs <- replicateM 7 (copyCard copper)
          emptyPlayer (i,(s,c)) = PlayerState i s c [] [] [] [] []
          emptyState = GameState (map emptyPlayer $ zip [0..] ps) supply
                       0 newTurn defaultGain c [QId 0..] [CId 0..]

data PlayerState = PlayerState {
      playerId       :: PId,
      playerName     :: String,
      playerChan     :: Chan MessageToClient,
      playerHand     :: [Card],
      playerDeck     :: [Card],
      playerDiscard  :: [Card],
      playerDuration :: [Card],
      playerMats     :: [(String,[Card])],

{- go code:
	supply := []SupplyPile{
		SupplyPile{10,Curse},
		SupplyPile{40,Copper},
		SupplyPile{40,Silver},
		SupplyPile{30,Gold},
		SupplyPile{8,Estate},
		SupplyPile{8,Duchy},
		SupplyPile{8,Province}};
-}


play :: Game [(String,Int)]
play = do winner <- endGame
          case winner of
            Just s -> return s
            Nothing -> turn >> play
    where endGame = do sup <- gets gameSupply
                       let empty = filter ((<=0).fst) sup
                           prov = fst $ filter (isProvince.snd) sup
                           over = length empty >= 3 || prov <= 0
                       if not over then return Nothing else do
                       ps <- gets gamePlayers
                       zip (map playerName ps) =<< map playerScore ps
          playerScore p = do cs <- allCards p
                             let ss = concatMap getScores $ map cardType cs
                             foldM (flip ($)) 0 ss
          isProvince = (=="Province") . cardName
          allCards p = playerHand p ++ playerDeck p ++ playerDiscard p
                       ++ playerDuration p ++ concatMap snd (playerMats p)
          getScores [] = []
          getScores (Score f:xs) = f:getScores xs
          getScores (_:xs) = getScores xs
                           
turn = :: Game ()
turn = do p <- gets currentTurn
          hand <- 
          cs <- askCards p -- ....?

          -- next turn
          n <- gets $ length . gamePlayers
          modify $ \s -> s { currentTurn = (p+1)`mod`n }