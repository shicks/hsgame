||| Merge >>>
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

<<< Merge |||
||| finish implementing base set >>>
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dominion.Stack ( OStack, UStack, IStack,
                        (*<<), (.<<), (<<), (*<#), (.<#), (<#),
                        (<*), (<.), (*<<<), (.<<<), (<<<), (#<), (#<#),
                        getStack, top, bottom, trash, shuffle, shuffleIO,
                        draw, hand, deck, discard, mat, durations, played,
                        allSupply, supplyCards, allCards,
                        defaultGain, gain ) where

import Dominion.Types
import Dominion.Question

import Prelude hiding ( mod )

import Control.Monad ( (>=>) )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Random ( randomRIO )
import Data.Array ( Ix, Array, elems, (//) )
import Data.List ( sortBy )
import Data.Maybe ( listToMaybe )
import Data.Ord ( comparing )

data OStack = OStack { -- ordered stack
      oAddToTop     :: [Card] -> Game (),  -- top is low
      oAddToBottom  :: [Card] -> Game (),
      oGetStack     :: Int -> Game [Card]  -- top is front
    }

orderedStack :: StackName -> OStack
orderedStack sn = OStack { oAddToTop = att, oAddToBottom = atb,
                           oGetStack = gs }
    where att :: [Card] -> Game ()
          att cs = do old <- gs'
                      let mindep = if null old
                                   then fromIntegral $ length cs
                                   else fst $ head old
                          upd depth c = (cardId c, (sn,depth,c))
                          depths = [mindep-1,mindep-2..]
                          updates = zipWith upd depths cs
                      modify $ \s -> s { gameCards = gameCards s//updates }
          atb :: [Card] -> Game ()
          atb cs = do old <- gs'
                      let maxdep = last $ (-1):map fst old -- default to 0
                          upd depth c = (cardId c, (sn,depth,c))
                          depths = [maxdep+1,maxdep+2..]
                          updates = zipWith upd depths cs
                      modify $ \s -> s { gameCards = gameCards s//updates }
          gs' = (sortBy cmpfst . concatMap issn . elems) `fmap` gets gameCards
          gs _ = map snd `fmap` gs'
          cmpfst = comparing fst
          issn (st,x,c) = if st == sn then [(x,c)] else []

data UStack = UStack { -- unordered stack
      uAddToStack   :: [Card] -> Game (),
      uGetStack    :: Int -> Game [Card]
    }

unorderedStack :: StackName -> UStack
unorderedStack sn = UStack { uAddToStack = add, uGetStack = gs }
    where add cs = do let upd c = (cardId c, (sn,fromIntegral $ cardId c,c))
                          updates = map upd cs
                      modify $ \s -> s { gameCards = gameCards s//updates }
          gs _ = (map snd . concatMap issn . elems) `fmap` gets gameCards
          issn (st,x,c) = if st == sn
                          then [(x,c)]
                          else []

class OrderedInputStack s where
    addToBottom :: s -> [Card] -> Game ()
    addToTop    :: s -> [Card] -> Game ()
class UnorderedInputStack s where
    addToStack :: s -> [Card] -> Game ()
class OutputStack s where
    unorderedGetStack :: s -> Int -> Game [Card]
class OutputStack s => OrderedOutputStack s where
    orderedGetStack :: s -> Int -> Game [Card]

instance OrderedInputStack OStack where
    addToBottom = oAddToBottom
    addToTop = oAddToTop
instance OrderedOutputStack OStack where
    orderedGetStack = oGetStack
instance OutputStack OStack where
    unorderedGetStack = oGetStack
instance UnorderedInputStack UStack where
    addToStack = uAddToStack
instance OutputStack UStack where
    unorderedGetStack = uGetStack

get :: OutputStack s => Int -> s -> Game [Card]
get = flip unorderedGetStack

data IStack s = IStack {
      inputProc :: [Card] -> Game (),
      origStack :: s
}
instance OrderedInputStack s => OrderedInputStack (IStack s) where
    addToBottom s cs = inputProc s cs >> addToBottom (origStack s) cs
    addToTop s cs = inputProc s cs >> addToTop (origStack s) cs
instance UnorderedInputStack s => UnorderedInputStack (IStack s) where
    addToStack s cs = inputProc s cs >> addToStack (origStack s) cs

-- mod :: Stack -> ([Card] -> [Card]) -> Game ()
-- mod = modifyStack

-- fromTop :: Stack -> Game Card
-- toTop :: Stack -> Card -> Game ()
-- fromBottom :: Stack -> Game Card
-- toBottom :: Stack -> Card -> Game ()

shuffle :: [a] -> Game [a]
shuffle as = liftIO $ shuffleIO as

shuffleIO :: [a] -> IO [a]
shuffleIO []  = return []
shuffleIO as = do i <- randomRIO (0,length as-1)
                  ((as!!i):) `fmap` shuffleIO (as!-i)
    where [] !- _ = []
          (a:as) !- 0 = as
          (a:as) !- n = a:(as!-(n-1))

getStack :: OutputStack s => s -> Game [Card]
getStack = get 0

infixr 5 *<<, .<<, <<
(*<<), (.<<) :: OrderedInputStack s => s -> [Card] -> Game ()
(*<<) = addToTop
(.<<) = addToBottom
(<<) :: UnorderedInputStack s => s -> [Card] -> Game ()
(<<)  = addToStack

-- These are to be combine with (<*) and (<.) to make e.g. *<#5<*
infixr 5 *<#, .<#, <#
(*<#), (.<#) :: OrderedInputStack s => s -> Game [Card] -> Game ()
(*<#) = (=<<) . (*<<)
(.<#) = (=<<) . (.<<)
(<#) :: UnorderedInputStack s => s -> Game [Card] -> Game ()
(<#)  = (=<<) . (<<)

-- This is silly too...
infixr 5 #<, #<#
(#<) :: ([Card] -> Game ()) -> [Card] -> Game [Card]
f #< cs = f cs >> return cs

(#<#) :: ([Card] -> Game ()) -> Game [Card] -> Game [Card]
(#<#) = (=<<) . (#<)

top, bottom :: OrderedOutputStack s => Int -> s -> Game [Card]
top n s = take n `fmap` orderedGetStack s n
bottom n s = (take n . reverse) `fmap` orderedGetStack s n

infixr 5 <., <*
(<.), (<*) :: OrderedOutputStack s => Int -> s -> Game [Card]
(<.) = bottom
(<*) = top

infix 5 .<<<, *<<<, <<<
(.<<<), (*<<<) :: (OrderedInputStack s1,OutputStack s2) => s1 -> s2 -> Game ()
(.<<<) to from = get 0 from >>= (to.<<)
(*<<<) to from = get 0 from >>= (to*<<)
(<<<) :: (UnorderedInputStack s1,OutputStack s2) => s1 -> s2 -> Game ()
(<<<) to from = get 0 from >>= (to<<)

draw :: Int -> PId -> Game ()
draw n p = do hand p <#n<* deck p
              h <- get 0 $ hand p -- put these in the stack modifiers now...
              tell p $ "Drew cards: hand="++show (h::[Card]) -- improve...

hand :: PId -> UStack
hand p = unorderedStack (SPId p "hand")

-- we've built in the reshuffling mechanism here...!
deck :: PId -> OStack
deck p = OStack att atb gs
    where OStack att atb gs0 = orderedStack (SPId p "deck")
          gs n = do x <- gs0 undefined {- ignored -}
                    if length x >= n
                       then return x
                       else do d <- get 0 $ discard p
                               d' <- shuffle d
                               atb d'
                               return $ x++d'

discard :: PId -> OStack
discard p = orderedStack (SPId p "discard")

mat :: String -> PId -> OStack
mat n p = orderedStack (SPId p ("stack-"++n))

durations :: PId -> UStack
durations p = unorderedStack (SPId p "durations")

played :: UStack
played = unorderedStack (SN "turnPlayed")

trash :: UStack
trash = unorderedStack $ SN "trash"

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
                  _  -> discard p *<< take 1 cs

gain :: PId -> (PId -> s) -> IStack s -- would map hookGain instead
gain p s = IStack (mapM_ (defaultGain p)) (s p)

-- gain :: PId -> Card -> Game ()
-- gain p c = join $ (($c) . ($p)) `fmap` gets hookGain
-- gain :: PId -> Card -> Game ()
-- gain = defaultGain

allCards :: PId -> Game [Card]
allCards p = (concatMap isp . elems) `fmap` gets gameCards
    where isp (st,_,c) = case st of
                           SPId p' _ | p' == p -> [c]
                           _ -> []

<<< finish implementing base set |||
