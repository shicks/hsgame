module Dominion.Stack ( ) where

import Dominion.Types
import Dominion.Unique ( copyCard )

import System.Random ( randomRIO )

type ModifyS m a = (a -> a) -> m a

class Stack s where
    mkStack :: String -> ModifyS Game [Card] -> Game [Card] -> s
instance Stack (Game [Card]) where
    mkStack _ _ g = g
instance Stack (ModifyS Game [Card]) where
    mkStack _ m _ = m
instance Stack String where
    mkStack s _ _ = s

-- fromTop :: Stack -> Game Card
-- toTop :: Stack -> Card -> Game ()
-- fromBottom :: Stack -> Game Card
-- toBottom :: Stack -> Card -> Game ()

shuffle :: [a] -> Game [a]
shuffle as = shuffle' (length as)
    where shuffle' 0 _  = return []
          shuffle' n as = do i <- liftIO $ randomRIO (0,n-1)
                             ((as!!i):) `fmap` shuffle' (n-1) (as!-i)
          [] !- _ = []
          (a:as) !- 0 = as
          (a:as) !- n = a:(as!-(n-1))

getStack :: Stack -> Game [Card]

*<< :: Stack s => s -> Card -> Game () -- put on top of stack
s *<< c = s (c:)

.<< :: Stack s => s -> Card -> Game () -- put on bottom of stack
s .<< c = s (++[c])

*<<& :: Stack s => s -> [Card] -> Game ()
(*<<&) s = mapM_ (s^<<)

.<<& :: Stack s => s -> [Card] -> Game ()
(.<<&) s = mapM_ (s.<<)

top :: Stack s => s -> Game (Maybe Card)
bottom :: Stack s => s -> Game (Maybe Card)

-- maybe return Game Bool?
.<<., .<<*, *<<., *<<* :: (Stack to, Stack from) => to -> from -> Game ()
(.<<.) = moveCard (.<<) bottom
(.<<*) = moveCard (.<<) top
(*<<.) = moveCard (*<<) bottom
(*<<*) = moveCard (*<<) top

-- utility function for (.<<.), etc
moveCard :: (Stack to, Stack from)
         => (to -> Card -> Game ()) -> (from -> Game (Maybe Card))
         -> to -> from -> Game ()
moveCard putCard getCard = do mc <- getCard t
                              case mc of
                                Nothing -> return ()
                                Just c  -> s `putCard` c

draw :: Int -> PId -> Game ()
draw n p = replicateM_ n $ hand p .<<* deck p

hand :: PId -> Stack
hand = \p ->
       mkStack ("hand "++show p')
               (\f -> withPlayer p' $ modify $
                      \s -> s { playerHand = f $ playerHand s })
               (gets $ playerHand . (!!p'))

-- we've built in the reshuffling mechanism here...!
deck :: PId -> Stack
deck = \p ->
       mkStack ("deck "++show p)
       (\f -> withPlayer p $ modify $
              \s -> s { playerDeck = f $ playerDeck s }) $
               do h <- gets $ playerDeck . (!!p)
                  if not $ null h then return h else do
                  d <- discardPile p
                  d' <- liftIO $ shuffle d
                  withPlayer p $ \s -> s { playerDeck=d', playerDiscard=[] }
                  return d'

discard :: PId -> Stack
discard = \p ->
          mkStack ("discard "++show p)
          (\f -> withPlayer p $ modify $
                 \s -> s { playerDiscard = f $ playerDiscard s })
          (gets $ playerDiscard . (!!p))

mat :: String -> Pid -> Stack
mat c = \p ->
        mkStack ("mat "++show p++" "++c)
        (\f -> withPlayer p $ modify $
               \s -> s { playerMats = modifys c f $ playerMats s })
        (gets $ maybe [] id . lookup c . playerMats . (!!p))
    where modifys c f [] = [(c,f [])]
          modifys c f ((c',x):ys) | c'==c     = (c',f s):ys
                                  | otherwise = (c',x):modifys c f ys

durations :: PId -> Stack
durations = \p ->
            mkStack ("durations "++show p)
            (\f -> withPlayer p $ modify $
                   \s -> s { playerDuration = f $ playerDuration s })
            (gets $ playerDuration . (!!p))

remove :: [Card] -> Stack -> Game ()
remove cs s = do mapM_ (\c -> s (rem c)) cs
    where rem c [] = []
          rem c (c':cs) | cardId c==cardId c' = cs
                        | otherwise = c':rem c cs

defaultGain :: PId -> Card -> Game ()
defaultGain p c = do modify $ \s -> s { gameSupply = f (gameSupply s) }
                     c' <- copyCard c
                     discard p *<< c
    where f [] = []
          f ((i,c'):cs) | cardName c' == cardName c = (i-1,c'):cs
                        | otherwise = (i,c'):f cs
