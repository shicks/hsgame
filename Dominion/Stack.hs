{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dominion.Stack ( Stack, (*<<), (.<<), (*<<&), (.<<&), (*<<@), (.<<@),
                        top, bottom, trash,
                        (.<<.), (.<<*), (*<<.), (*<<*), (.<<<), (*<<<),
                        draw, hand, deck, discard, mat, durations, played,
                        remove, defaultGain ) where

import Dominion.Types
import Dominion.Question

import Control.Monad ( replicateM_ )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Random ( randomRIO )

type ModifyS m a = (a -> a) -> m ()

class Stack s where
    mkStack :: String -> ModifyS Game [Card] -> Game [Card] -> s
instance Stack (Game [Card]) where
    mkStack _ _ g = g
instance Stack (ModifyS Game [Card]) where
    mkStack _ m _ = m
instance Stack String where
    mkStack s _ _ = s

type StackRW = (Game [Card], ModifyS Game [Card])
instance Stack StackRW where
    mkStack _ m g = (g,m)

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

(*<<) :: ModifyS Game [Card] -> Card -> Game () -- put on top of stack
s *<< c = s (c:)

(.<<) :: ModifyS Game [Card] -> Card -> Game () -- put on bottom of stack
s .<< c = s (++[c])

(*<<&) :: ModifyS Game [Card] -> ([Card],StackRW) -> Game ()
(*<<&) s1 (cs,s2) = do cs' <- remove' cs s2
                       s1 *<<@ cs'

(.<<&) :: ModifyS Game [Card] -> ([Card],StackRW) -> Game ()
(.<<&) s1 (cs,s2) = do cs' <- remove' cs s2
                       s1 .<<@ cs'

-- these are unconditional adds
(*<<@) :: ModifyS Game [Card] -> [Card] -> Game ()
(*<<@) s = mapM_ (s*<<)

(.<<@) :: ModifyS Game [Card] -> [Card] -> Game ()
(.<<@) s = mapM_ (s.<<)

top :: StackRW -> Game (Maybe Card)
top s = do cs <- fst s
           case cs of
             (x:xs) -> snd s (\_ -> xs) >> return (Just x)
             [] -> return Nothing

bottom :: StackRW -> Game (Maybe Card)
bottom s = do cs <- fst s
              let (xs,x) = last cs
              snd s $ \_ -> xs
              return x
    where last (y:x:xs) = let (ys,l) = last (x:xs) in (y:ys,l)
          last [x] = ([],Just x)
          last [] = ([],Nothing)

-- maybe return Game Bool?
(.<<.), (.<<*), (*<<.), (*<<*) :: ModifyS Game [Card] -> StackRW -> Game ()
(.<<.) = moveCard (.<<) bottom
(.<<*) = moveCard (.<<) top
(*<<.) = moveCard (*<<) bottom
(*<<*) = moveCard (*<<) top

(.<<<), (*<<<) :: ModifyS Game [Card] -> StackRW -> Game ()
(.<<<) = moveAllCards (.<<)
(*<<<) = moveAllCards (*<<)

-- utility function for (.<<.), etc
moveCard :: (ModifyS Game [Card] -> Card -> Game ())
         -> (StackRW -> Game (Maybe Card))
         -> ModifyS Game [Card] -> StackRW -> Game ()
moveCard putCard getCard to from = do mc <- getCard from
                                      case mc of
                                        Nothing -> return ()
                                        Just c  -> to `putCard` c

moveAllCards :: (ModifyS Game [Card] -> Card -> Game ())
             -> ModifyS Game [Card] -> StackRW -> Game ()
moveAllCards putCard to from = do cs <- fst from
                                  if null cs then return () else do
                                  moveCard putCard top to from
                                  moveAllCards putCard to from

draw :: Int -> PId -> Game ()
draw n p = do replicateM_ n $ hand p .<<* deck p
              h <- hand p
              tell p $ "Drew cards: hand="++show (h::[Card]) -- improve...

hand :: Stack s => PId -> s
hand = \p ->
       mkStack ("hand "++show p)
               (\f -> withPlayer p $ modify $
                      \s -> s { playerHand = f $ playerHand s })
               (withPlayer p $ gets playerHand)

-- we've built in the reshuffling mechanism here...!
deck :: Stack s => PId -> s
deck = \p ->
       mkStack ("deck "++show p)
       (\f -> withPlayer p $ modify $
              \s -> s { playerDeck = f $ playerDeck s }) $
       do h <- withPlayer p $ gets playerDeck
          if not $ null h then return h else do
          d <- discard p
          d' <- shuffle d
          withPlayer p $ modify $ \s -> s { playerDeck=d', playerDiscard=[] }
          return d'

discard :: Stack s => PId -> s
discard = \p ->
          mkStack ("discard "++show p)
          (\f -> withPlayer p $ modify $
                 \s -> s { playerDiscard = f $ playerDiscard s })
          (withPlayer p $ gets playerDiscard)

mat :: Stack s => String -> PId -> s
mat c = \p ->
        mkStack ("mat "++show p++" "++c)
        (\f -> withPlayer p $ modify $
               \s -> s { playerMats = modifys c f $ playerMats s })
        (withPlayer p $ gets $ maybe [] id . lookup c . playerMats)
    where modifys c f [] = [(c,f [])]
          modifys c f ((c',x):ys) | c'==c     = (c',f x):ys
                                  | otherwise = (c',x):modifys c f ys

durations :: Stack s => PId -> s
durations = \p ->
            mkStack ("durations "++show p)
            (\f -> withPlayer p $ modify $
                   \s -> s { playerDuration = f $ playerDuration s })
            (withPlayer p $ gets playerDuration)

played :: Stack s => s
played = mkStack ("played")
         (\f -> withTurn $ modify $
                \s -> s { turnPlayed = f $ turnPlayed s })
         (gets $ turnPlayed . turnState)

remove :: [Card] -> ModifyS Game [Card] -> Game ()
remove cs s = mapM_ (\c -> s (rem c)) cs
    where rem c [] = []
          rem c (c':cs) | cardId c==cardId c' = cs
                        | otherwise = c':rem c cs

trash :: Card -> ModifyS Game [Card] -> Game ()
trash c = remove [c]

remove' :: [Card] -> StackRW -> Game [Card]
remove' cs s = concat `fmap` mapM (r' s) cs
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
