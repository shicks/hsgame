{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Dominion.Types ( GameState(..), PlayerState(..), Game,
                        runGame, evalGame, execGame, try,
                        GameError(),
                        withTurn, withPlayer, TurnState(..),
                        StackName(..),
                        MessageToServer(..), RegisterQuestionMessage(..),
                        MessageToClient(..), ResponseFromClient(..),
                        Card(..), CardType(..), HookType(..),
                        runSetupHooks,
                        Answer(..), pickCard,
                        CardDescription(..), describeCard, lookupCard,
                        QuestionMessage(..), InfoMessage(..),
                        newQId,
                        getSelf, getLHO, getRHO, getOpponents, getAllPlayers,
                        Attack, Reaction,
                        QId, CId, PId(..) ) where

import TCP.Chan ( ShowRead, Input, Output )
import Control.Monad.State ( StateT(..), MonadState,
                             get, gets, put, modify, liftIO )
import Control.Monad.Error ( MonadError, Error(..),
                             catchError, throwError )
import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad ( when )
import Data.Array ( Array, Ix )

-- Plan: throw in an ErrorT on the outside, so that we can use
-- a "try" structure to catch pattern match errors:
--   try :: (Monad m,Error e) => ErrorT e m a -> ErrorT e m (Either e a)
--   try job = lift $ runErrorT job
-- We could also rethrow anything that *isn't* a pattern match error...

-- type Game = StateT GameState IO

-- Did I mix up the threading...?
newtype GameError = GameError String deriving ( Eq, Show, Error )
newtype Game a = Game {
      runGame :: GameState -> IO (Either GameError a,GameState)
    }

evalGame :: Game a -> GameState -> IO (Either GameError a)
evalGame g s = fst `fmap` runGame g s

execGame :: Game a -> GameState -> IO GameState
execGame g s = snd `fmap` runGame g s

try :: Game a -> Game ()
try a = catchError (a >> return ()) (\_ -> return ())

instance Monad Game where
    return a = Game $ \s -> return (Right a,s)
    fail e = Game $ \s -> return (Left $ error e,s)
    Game a >>= f = Game $ \s -> do (a',s') <- a s
                                   case a' of
                                     Left e -> return (Left e,s')
                                     Right a'' -> runGame (f a'') s'

instance MonadState GameState Game where
    get = Game $ \s -> return (Right s,s)
    put s = Game $ \_ -> return (Right (),s)

instance MonadError GameError Game where
    throwError e = Game $ \s -> return (Left e,s)
    catchError (Game a) f = Game $ \s -> do (a',s') <- a s
                                            case a' of
                                              Left e -> runGame (f e) s'
                                              _ -> return (a',s')

instance MonadIO Game where
    liftIO a = Game $ \s -> do { a' <- a; return (Right a',s) }

instance Functor Game where
    fmap f (Game a) = Game $ \s -> do (a',s') <- a s
                                      return (f `fmap` a',s')

data GameState = GameState {
      gamePlayers  :: [PlayerState],
      gameCards    :: Array CId (StackName, Integer, Card),
      currentTurn  :: PId,
      turnState    :: TurnState,
--      hookGain     :: PId -> Card -> Game (),  {- for smuggler -}
--      hookBuy      :: PId -> Card -> Game (),  {- for embargo, treasury -}
      inputChan    :: Input MessageToServer,
      outputChan   :: Output RegisterQuestionMessage,
      _qIds        :: [QId]  -- [QId 0..]
    }

data StackName = SN String | SPId PId String
                 deriving ( Eq )

data PlayerState = PlayerState {
      playerId        :: PId,
      playerName      :: String,
      playerChan      :: Output MessageToClient,
      durationEffects :: [Game ()]
    }

data TurnState = TurnState {
      turnActions  :: Int,
      turnBuys     :: Int,
      turnCoins    :: Int,
      priceMod     :: Card -> Int,
      treasureMod  :: Card -> Int,
      cleanupHooks :: [Game () -> Game ()]
        -- ORDERED: treasury first, then outpost
        -- outpost needs to know how to take another turn
        -- treasury needs to know whether we bought a victory... :-/
        -- embargo also piggybacks only on buys, so these can go together.
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
    show (Card id_ pr name text_ typ_) =name++" ("++show pr++")" -- ++": "++text
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
    = Action (Card -> Maybe Card -> Game ())
    | Victory
    | Treasure Int
    | Reaction Reaction
    | Score (Int -> Game Int)
    | Hook HookType

instance Show CardType where
    show (Action _) = "Action"
    show Victory = "Victory"
    show (Treasure n) = "Treasure"
    show (Reaction _) = "Reaction"
    show _ = ""

data HookType = SetupHook ([Card] -> Game ())

runSetupHooks :: [Card] -> Card -> Game ()
runSetupHooks cs c = mapM_ ($cs) [g | Hook (SetupHook g) <- cardType c]

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
newtype CId = CId Int deriving ( Real, Integral, Num, Eq, Ord, Enum,
                                 Show, Read, Ix ) -- Card

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

data InfoMessage = InfoMessage String
                 | GameOver String
                 | CardPlay String [CardDescription] -- first String is player
                 | CardDraw String (Either Int [CardDescription]) -- may not be public
                 | CardDiscard String [CardDescription]
                 | CardTrash String [CardDescription]
                 | CardReveal String [CardDescription] String -- from where?
                 | CardBuy String [CardDescription]
                 deriving ( Show, Read )
data QuestionMessage
    = SelectAction | SelectReaction String           -- from hand
    | SelectSupply String | SelectBuys | SelectGain  -- from supply
    | DiscardBecause String | UndrawBecause String   -- maybe Card instead?
    | TrashBecause String
    | GiveAway String                                -- e.g. Masquerade
    | Gain String                                    -- e.g. black market
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

getSelf :: Game PId
getSelf = gets currentTurn

getLHO :: PId -> Game PId
getLHO p = do n <- gets $ length . gamePlayers
              return $ (p+PId 1) `mod` PId n

getRHO :: PId -> Game PId
getRHO p = do n <- gets $ length . gamePlayers
              return $ (p+PId n-PId 1) `mod` PId n

getOpponents :: PId -> Game [PId]
getOpponents p = do n <- gets $ length . gamePlayers
                    return $ filter (/=p) $ map PId [0..n-1]

getAllPlayers :: Game [PId]
getAllPlayers = do n <- gets $ length . gamePlayers
                   return $ map PId [0..n-1]

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
