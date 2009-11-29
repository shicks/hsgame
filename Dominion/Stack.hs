||| Merge >>>
module Dominion.Stack ( ) where

import Dominion.Types
import Dominion.Unique ( copyCard )

import Data.Random ( randomRIO )

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

<<< Merge |||
||| Merge stupidly? >>>
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dominion.Stack ( Stack, (*<<), (.<<), (*<<&), (.<<&), (*<<@), (.<<@),
                        getStack, top, bottom, trash,
                        (.<<.), (.<<*), (*<<.), (*<<*), (.<<<), (*<<<),
                        draw, hand, deck, discard, mat, durations, played,
                        remove, defaultGain, allCards ) where

import Dominion.Types
import Dominion.Question

import Prelude hiding ( mod )

import Control.Monad ( replicateM_ )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Random ( randomRIO )

data Stack = Stack {
      stackName    :: String,
      modifyStack  :: ([Card] -> [Card]) -> Game (),
      getStack     :: Game [Card]
    }

get :: Stack -> Game [Card]
get = getStack
mod :: Stack -> ([Card] -> [Card]) -> Game ()
mod = modifyStack

-- fromTop :: Stack -> Game Card
-- toTop :: Stack -> Card -> Game ()
-- fromBottom :: Stack -> Game Card
-- toBottom :: Stack -> Card -> Game ()

shuffle :: [a] -> Game [a]
shuffle []  = return []
shuffle as = do i <- liftIO $ randomRIO (0,length as-1)
                ((as!!i):) `fmap` shuffle (as!-i)
    where [] !- _ = []
          (a:as) !- 0 = as
          (a:as) !- n = a:(as!-(n-1))

-- getStack :: Stack -> Game [Card]

(*<<) :: Stack -> Card -> Game () -- put on top of stack
s *<< c = mod s (c:)

(.<<) :: Stack -> Card -> Game () -- put on bottom of stack
s .<< c = mod s (++[c])

(*<<&) :: Stack -> ([Card],Stack) -> Game ()
(*<<&) s1 (cs,s2) = do cs' <- remove' cs s2
                       s1 *<<@ cs'

(.<<&) :: Stack -> ([Card],Stack) -> Game ()
(.<<&) s1 (cs,s2) = do cs' <- remove' cs s2
                       s1 .<<@ cs'

-- these are unconditional adds
(*<<@) :: Stack -> [Card] -> Game ()
(*<<@) s = mapM_ (s*<<)

(.<<@) :: Stack -> [Card] -> Game ()
(.<<@) s = mapM_ (s.<<)

top :: Stack -> Game (Maybe Card)
top s = do cs <- get s
           case cs of
             (x:xs) -> mod s (\_ -> xs) >> return (Just x)
             [] -> return Nothing

bottom :: Stack -> Game (Maybe Card)
bottom s = do cs <- get s
              let (xs,x) = last cs
              mod s $ \_ -> xs
              return x
    where last (y:x:xs) = let (ys,l) = last (x:xs) in (y:ys,l)
          last [x] = ([],Just x)
          last [] = ([],Nothing)

-- maybe return Game Bool?
(.<<.), (.<<*), (*<<.), (*<<*) :: Stack -> Stack -> Game ()
(.<<.) = moveCard (.<<) bottom
(.<<*) = moveCard (.<<) top
(*<<.) = moveCard (*<<) bottom
(*<<*) = moveCard (*<<) top

(.<<<), (*<<<) :: Stack -> Stack -> Game ()
(.<<<) = moveAllCards (.<<)
(*<<<) = moveAllCards (*<<)

-- utility function for (.<<.), etc
moveCard :: (Stack -> Card -> Game ())
         -> (Stack -> Game (Maybe Card))
         -> Stack -> Stack -> Game ()
moveCard putCard getCard to from = do mc <- getCard from
                                      case mc of
                                        Nothing -> return ()
                                        Just c  -> to `putCard` c

moveAllCards :: (Stack -> Card -> Game ()) -> Stack -> Stack -> Game ()
moveAllCards putCard to from = do cs <- get from
                                  if null cs then return () else do
                                  moveCard putCard top to from
                                  moveAllCards putCard to from

draw :: Int -> PId -> Game ()
draw n p = do replicateM_ n $ hand p .<<* deck p
              h <- get $ hand p
              tell p $ "Drew cards: hand="++show (h::[Card]) -- improve...

hand :: PId -> Stack
hand = \p ->
       Stack ("hand "++show p)
             (\f -> withPlayer p $ modify $
                    \s -> s { playerHand = f $ playerHand s })
             (withPlayer p $ gets playerHand)

-- we've built in the reshuffling mechanism here...!
deck :: PId -> Stack
deck = \p ->
       Stack ("deck "++show p)
       (\f -> withPlayer p $ modify $
              \s -> s { playerDeck = f $ playerDeck s }) $
       do h <- withPlayer p $ gets playerDeck
          if not $ null h then return h else do
          d <- get $ discard p
          d' <- shuffle d
          withPlayer p $ modify $ \s -> s { playerDeck=d', playerDiscard=[] }
          return d'

discard :: PId -> Stack
discard = \p ->
          Stack ("discard "++show p)
          (\f -> withPlayer p $ modify $
                 \s -> s { playerDiscard = f $ playerDiscard s })
          (withPlayer p $ gets playerDiscard)

mat :: String -> PId -> Stack
mat c = \p ->
        Stack ("mat "++show p++" "++c)
        (\f -> withPlayer p $ modify $
               \s -> s { playerMats = modifys c f $ playerMats s })
        (withPlayer p $ gets $ maybe [] id . lookup c . playerMats)
    where modifys c f [] = [(c,f [])]
          modifys c f ((c',x):ys) | c'==c     = (c',f x):ys
                                  | otherwise = (c',x):modifys c f ys

mats :: PId -> Game [(String,Stack)]
mats p = map (\m -> (m,mat m p)) `fmap`
         (withPlayer p $ gets $ map fst . playerMats)

durations :: PId -> Stack
durations = \p ->
            Stack ("durations "++show p)
            (\f -> withPlayer p $ modify $
                   \s -> s { playerDuration = f $ playerDuration s })
            (withPlayer p $ gets playerDuration)

played :: Stack
played = Stack ("played")
         (\f -> withTurn $ modify $
                \s -> s { turnPlayed = f $ turnPlayed s })
         (gets $ turnPlayed . turnState)

remove :: [Card] -> Stack -> Game ()
remove cs s = mapM_ (\c -> mod s (rem c)) cs
    where rem c [] = []
          rem c (c':cs) | cardId c==cardId c' = cs
                        | otherwise = c':rem c cs



-- *this is just a silly synonym for remove
trash :: Card -> Stack -> Game ()
trash c = remove [c]

-- *this is another version fo remove that tells what was
-- successfully removed; we use it in (*<<&) so that only
-- the removed cards get moved...
remove' :: [Card] -> Stack -> Game [Card]
remove' cs s = concat `fmap` mapM (r' (get s,mod s)) cs
    where r' (r,w) c = do h <- r
                          if c `elem` h
                             then do w $ rem c
                                     return [c]
                             else return []
          rem c [] = []
          rem c (c':cs) | cardId c==cardId c' = cs
                        | otherwise = c':rem c cs

defaultGain :: PId -> Card -> Game ()
defaultGain p c = do modify $ \s -> s { gameSupply = f (gameSupply s) }
                     c' <- copyCard c
                     discard p *<< c
    where f [] = []
          f ((c',i):cs) | cardName c' == cardName c = (c',i-1):cs
                        | otherwise = (c',i):f cs

allCards :: PId -> Game [Card]
allCards p = withPlayer p $ do
               p' <- gets id
               return $ playerHand p' ++ playerDeck p' ++ playerDiscard p'
                        ++ playerDuration p' ++ concatMap snd (playerMats p')

-- allCards :: PId -> Game [Card]
-- allCards p = do xs <- mapM (\s -> get $ s p) [deck, hand, discard, durations]
--                 ys <- mapM (\s -> get $ snd s) =<< mats p
--                 return $ concat xs++concat ys

<<< Merge stupidly? |||
