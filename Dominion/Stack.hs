{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dominion.Stack ( Stack, (*<<), (.<<), (*<<&), (.<<&), (*<<@), (.<<@),
                        getStack, top, bottom, trash, shuffle, shuffleIO,
                        (.<<.), (.<<*), (*<<.), (*<<*), (.<<<), (*<<<),
                        draw, hand, deck, discard, mat, durations, played,
                        allSupply, supplyCards,
                        remove, defaultGain, allCards ) where

import Dominion.Types
import Dominion.Question

import Prelude hiding ( mod )

import Control.Monad ( replicateM_ )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Random ( randomRIO )
import Data.Array ( Ix, Array, elems, (//) )
import Data.List ( sortBy )

data Stack = Stack {
      modifyStack  :: ([Card] -> [Card]) -> Game (),
      getStack     :: Game [Card]
    }

anyStack :: StackName -> Stack
anyStack sn = Stack { modifyStack = ms, getStack = gs }
    where ms f = do new <- f `fmap` gs
                    let upd depth c = (cardId c, (sn,depth,c))
                        depths = [0..]
                        updates = zipWith upd depths new
                    modify $ \s -> s { gameCards = gameCards s//updates }
          gs = (reverse . map snd . sortBy cmpfst .
                        concatMap issn . elems) `fmap` gets gameCards
          cmpfst x y = compare (fst x) (fst y)
          issn (st,x,c) = if st == sn
                          then [(x,c)]
                          else []

get :: Stack -> Game [Card]
get = getStack
mod :: Stack -> ([Card] -> [Card]) -> Game ()
mod = modifyStack

-- fromTop :: Stack -> Game Card
-- toTop :: Stack -> Card -> Game ()
-- fromBottom :: Stack -> Game Card
-- toBottom :: Stack -> Card -> Game ()

shuffle :: [a] -> Game [a]
shuffle as = liftIO $ shuffleIO as

shuffleIO :: [a] -> IO [a]
shuffleIO []  = return []
shuffleIO as = do i <- randomRIO (0,length as-1)
                  ((as!!i):) `fmap` shuffleIO (as\\i)
    where [] \\ _ = []
          (a:as) \\ 0 = as
          (a:as) \\ n = a:(as\\(n-1))

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
hand p = anyStack (SPId p "hand")

-- we've built in the reshuffling mechanism here...!
deck :: PId -> Stack
deck p = Stack ms0 gs
    where Stack ms0 gs0 = anyStack (SPId p "deck")
          gs = do x <- gs0
                  if not (null x)
                     then return x
                     else do d <- get $ discard p
                             d' <- shuffle d
                             ms0 (const d')
                             return d'

discard :: PId -> Stack
discard p = anyStack (SPId p "discard")

mat :: String -> PId -> Stack
mat n p = anyStack (SPId p ("stack-"++n))

durations :: PId -> Stack
durations p = anyStack (SPId p "durations")

played :: Stack
played = anyStack (SN "turnPlayed")

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

allSupply :: Game [Card]
allSupply = (concatMap iss . elems) `fmap` gets gameCards
    where iss (SN "supply",_,c) = [c]
          iss _ = []

supplyCards :: String -> Game [Card]
supplyCards n = filter ((==n) . cardName) `fmap` allSupply

defaultGain :: PId -> Card -> Game ()
defaultGain p c0 =
    do cs <- supplyCards (cardName c0)
       case cs of [] -> fail "cannot gain card with empty supply"
                  c:_ -> discard p *<< c

allCards :: PId -> Game [Card]
allCards p = (concatMap isp . elems) `fmap` gets gameCards
    where isp (st,_,c) = case st of
                           SPId p' _ | p' == p -> [c]
                           _ -> []
