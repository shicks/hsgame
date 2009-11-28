module Dominion.Types ( ) where

import Control.Concurrent.Chan ( Chan, writeChan, readChan )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar )

type Game = StateT IO GameState

-- startGame :: ... -> IO ()
-- stateGame = do c <- newChan
--                forkIO $ readFrom c
--                GameState { inputChan = c }

data GameState = GameState {
      gamePlayers  :: [PlayerState],
      gameSupply   :: [(Card,Int)],
      currentTurn  :: PId,
      turnState    :: TurnState,
      hookGain     :: Card -> PlayerId -> Game (),
      inputChan    :: Chan MessageToServer,
      _qIds        :: [QId],  -- [QId 0..]
      _cIds        :: [CId],  -- [CId 0..]
    }

data PlayerState = PlayerState {
      playerId       :: PId,
      playerName     :: String,
      playerChan     :: Chan MessageToClient,
      playerHand     :: [Card],
      playerDeck     :: [Card],
      playerDiscard  :: [Card],
      playerDuration :: [Card],
      playerMats     :: [(String,[Card])],
    }

data TurnState = TurnState {
      turnActions  :: Int,
      turnBuys     :: Int,
      turnCoins    :: Int,
      turnPriceMod :: Card -> Int,
}

data Card = Card {
      cardId    :: CId,
      cardName  :: String,
      cardText  :: String,
      cardType  :: [CardType],
      cardPrice :: Int,
    }

data CardType
    = Action (Game ())
    | Victory
    | Treasure Int
    | Reaction Reaction
    | Score (Int -> Game -> Int)

type PId = Int -- Player
newtype QId = QId Int deriving ( Num, Eq, Ord, Enum, Show, Read ) -- Question
newtype CId = CId Int deriving ( Num, Eq, Ord, Enum, Show, Read ) -- Card

data MessageToClient = Info InfoMessage
                     | Question QId QuestionMessage [Answer] (Int,Int)
data MessageToServer = AnswerFromClient QId [Answer]
                     | RegisterQuestion QId ([Answer] -> Game Bool)

data Answer = PickCard Card | Choose String  deriving ( Eq, Show )
data InfoMessage = InfoMessage String        deriving ( Show )
data QuestionMessage
    = SelectAction | SelectReaction String           -- from hand
    | SelectSupply String | SelectBuy | SelectGain   -- from supply
    | DiscardBecause String                          -- maybe Card instead?
    | TrashBecause String
    | OtherQuestion String                           -- e.g. envoy?
    deriving ( Show )



-- self :: Game PId
-- self = gets currentTurn

withTurn :: StateT IO TurnState a -> Game a
withTurn job = do s <- gets turnState
                  (s',a) <- liftIO $ runStateT job s
                  modify $ \ss -> ss { turnState = s' }
                  return a

withPlayer :: Player p => p -> StateT IO PlayerState a -> Game a
withPlayer n job = do n' <- toP n
                      p <- gets $ (!!n') . playerState
                      (p',a) <- liftIO $ runStateT job p
                      modify $ \s -> s { playerState = mod p' n' s }
                      return a
    where mod _ _ [] = [] -- fail "withPlayer: invalid PId"?
          mod p' 0 (_:ss) = (p':ss)
          mod p' n (s:ss) = s:mod p' (n-1) ss



-- class Player p where
--     toP :: p -> Game PId
-- instance Player (Game PId) where toP = id
-- instance Player PId where toP = return

-- withP :: Player p => p -> (PId -> Game a) -> Game a
-- withP p f = do { p' <- toP p; f p' }

-- class G b a | a -> b where
--     toG :: a -> Game b

-- instance G Card (Game Card) where toG = id
-- instance G Card Card where toG = return

-- instance G [Card] (Game [Card]) where toG = id
-- instance G [Card] [Card] where toG = return

-- instance G PId (Game PId) where toG = id
-- instance G PId PId where toG = return

-- withG :: G a b => (a -> Game c) -> b -> Game c
-- withG f b = do { a <- b; f a }
