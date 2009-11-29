{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dominion.Types ( GameState(..), PlayerState(..), Game,
                        withTurn, withPlayer, TurnState(..),
                        MessageToServer(..), RegisterQuestionMessage(..),
                        MessageToClient(..), ResponseFromClient(..),
                        Card(..), CardType(..),
                        Answer(..), pickCard,
                        CardDescription(..), describeCard, lookupCard,
                        QuestionMessage(..), InfoMessage(..),
                        newQId, newCId, copyCard, getSelf,
                        Attack, Reaction,
                        QId, CId, PId(..) ) where

import TCP.Chan ( ShowRead, Input, Output )
import Control.Monad.State ( StateT, runStateT, gets, modify, liftIO )
import Control.Monad ( when )

type Game = StateT GameState IO

-- startGame :: ... -> IO ()
-- stateGame = do c <- newChan
--                forkIO $ readFrom c
--                GameState { inputChan = c }

data GameState = GameState {
      gamePlayers  :: [PlayerState],
      gameSupply   :: [(Card,Int)],
      currentTurn  :: PId,
      turnState    :: TurnState,
      hookGain     :: PId -> Card -> Game (),
      inputChan    :: Input MessageToServer,
      outputChan   :: Output RegisterQuestionMessage,
      _qIds        :: [QId],  -- [QId 0..]
      _cIds        :: [CId]   -- [CId 0..]
    }

data PlayerState = PlayerState {
      playerId        :: PId,
      playerName      :: String,
      playerChan      :: Output MessageToClient,
      playerHand      :: [Card],
      playerDeck      :: [Card],
      playerDiscard   :: [Card],
      playerDuration  :: [Card],
      playerMats      :: [(String,[Card])],
      durationEffects :: [Game ()]
    }

data TurnState = TurnState {
      turnActions  :: Int,
      turnBuys     :: Int,
      turnCoins    :: Int,
      turnPriceMod :: Card -> Int,
      turnPlayed   :: [Card]
}

data Card = Card {
      cardId    :: CId,
      cardPrice :: Int,
      cardName  :: String,
      cardText  :: String,
      cardType  :: [CardType]
    }
instance Eq Card where
    Card i _ _ _ _ == Card j _ _ _ _ = i==j
instance Show Card where
    show (Card id_ pr name text_ typ_) = show (pr,name)
        -- '(':show pr++") "++name -- ++": "++text
data CardDescription =
 CardDescription { cid :: CId, cprice :: Int, cname :: String, ctext :: String }
                 deriving ( Eq, Show, Read )
instance ShowRead CardDescription

describeCard :: Card -> CardDescription
describeCard (Card a b c d _) = CardDescription a b c d

pickCard :: Card -> Answer
pickCard = PickCard . describeCard

lookupCard :: CardDescription -> [Card] -> Maybe Card
lookupCard d cs = do c:_ <- Just $ filter ((== cid d) . cardId) cs
                     Just c

data CardType
    = Action (Game ())
    | Victory
    | Treasure Int
    | Reaction Reaction
    | Score (Int -> Game Int)

instance Show CardType where
    show (Action _) = "Action"
    show Victory = "Victory"
    show (Treasure n) = "Treasure"
    show (Reaction _) = "Reaction"
    show _ = ""

-- *How to actually perform the attack.  This is slightly tricky, since
-- some attacks depend on choices made by attacker...
type Attack = PId      -- ^attacker
            -> PId     -- ^defender
            -> Game ()

-- *Basic reaction type is @Attack -> Attack@.  But @Reaction@ type asks
-- the attacked player what to do, and gives a continuation in case the
-- attack is still unresolved.  @Duration@s can "install" @Reaction@s as
-- well.
type Reaction = PId                       -- ^defender
              -> Game (Attack -> Attack)  -- ^continuation
              -> Game (Attack -> Attack)

newtype PId = PId Int deriving ( Real, Integral, Num, Eq, Ord, Enum,
                                 Show, Read ) -- Player
newtype QId = QId Int deriving ( Num, Eq, Ord, Enum, Show, Read ) -- Question
newtype CId = CId Int deriving ( Num, Eq, Ord, Enum, Show, Read ) -- Card

data MessageToClient = Info InfoMessage
                     | Question QId QuestionMessage [Answer] (Int,Int)
                       deriving ( Show, Read )
instance ShowRead MessageToClient
data MessageToServer = AnswerFromClient QId [Answer]
                     | RegisterQuestion QId ([Answer] -> IO Bool)
data RegisterQuestionMessage = RQ QId ([Answer] -> IO Bool)

data Answer = PickCard CardDescription
            | Choose String  deriving ( Eq, Show, Read )

data ResponseFromClient = ResponseFromClient QId [Answer]
                          deriving ( Show, Read )
instance ShowRead ResponseFromClient

data InfoMessage = InfoMessage String        deriving ( Show, Read )
data QuestionMessage
    = SelectAction | SelectReaction String           -- from hand
    | SelectSupply String | SelectBuy | SelectGain   -- from supply
    | DiscardBecause String | UndrawBecause String   -- maybe Card instead?
    | TrashBecause String
    | OtherQuestion String                           -- e.g. envoy?
    deriving ( Show, Read )



-- self :: Game PId
-- self = gets currentTurn

withTurn :: StateT TurnState IO a -> Game a
withTurn job = do s <- gets turnState
                  (a,s') <- liftIO $ runStateT job s
                  modify $ \ss -> ss { turnState = s' }
                  return a

withPlayer :: PId -> StateT PlayerState IO a -> Game a
withPlayer (PId n) job = do ps <- gets gamePlayers
                            when (n>=length ps) $
                                 fail $ "withPlayer: invalid PId: "++show n
                            let p = ps!!n 
                            (a,p') <- liftIO $ runStateT job p
                            modify $ \s -> s { gamePlayers = mod p' n $
                                                             gamePlayers s }
                            return a
    where mod _ _ [] = [] -- fail "withPlayer: invalid PId"?
          mod p' 0 (_:ss) = (p':ss)
          mod p' n (s:ss) = s:mod p' (n-1) ss


newQId :: Game QId
newQId = do qs <- gets _qIds
            modify $ \s -> s { _qIds = tail qs }
            return $ head qs

newCId :: Game CId
newCId = do cs <- gets _cIds
            modify $ \s -> s { _cIds = tail cs }
            return $ head cs

copyCard :: Card -> Game Card
copyCard c = do u <- newCId
                return $ c { cardId = u }

getSelf :: Game PId
getSelf = gets currentTurn

left :: PId -> Game PId
left p = do n <- gets $ length . gamePlayers
            return $ (p+PId 1) `mod` PId n

right :: PId -> Game PId
right p = do n <- gets $ length . gamePlayers
             return $ (p+PId n-PId 1) `mod` PId n

fromPId :: PId -> Game PlayerState
fromPId p = withPlayer p $ gets id

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
