{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dominion.Stack ( OStack, UStack, IStack, printStack,
                        addCards, orderedStack, unorderedStack,
                        (*<<), (.<<), (<<), (*<#), (.<#), (<#),
                        (<*), (<.), (*<<<), (.<<<), (<<<), (#<), (#<#),
                        stackName, getStack, top, bottom,
                        shuffle, shuffleIO,
                        draw, hand, deck, discard, mat, trash,
                        durations, played, prevDuration, aside,
                        allSupply, supplyCards, allCards,
                        defaultGain, gain, cardWhere ) where

import Dominion.Types
import Dominion.Question

import Prelude hiding ( mod )

import Control.Monad ( (>=>) )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Random ( randomRIO )
import Data.Array ( Ix, Array, elems, listArray, bounds, (//), (!) )
import Data.List ( sortBy )
import Data.Maybe ( listToMaybe )
import Data.Ord ( comparing )

addCards :: [Card] -> Game [Card]
addCards cs = do cur <- gets gameCards
                 let (mn,mx) = bounds cur
                     ids = [mx+1,mx+2..]
                     upd i c = c { cardId = i }
                     cs' = zipWith upd ids cs
                     upd' c = (SN "supply",fromIntegral $ cardId c,c)
                     cur' = listArray (mn,mx+fromIntegral (length cs))
                            (elems cur++map upd' cs')
                 modify $ \s -> s { gameCards = cur' }
                 return cs'

-- for debugging
printStack :: OutputStack s => String -> s -> Game ()
printStack s x = do cs <- getStack x
                    let pretty c = cardName c++" ("++show (cardPrice c)
                                   ++") ["++show (cardId c)++"]"
                    liftIO $ putStrLn s
                    liftIO $ mapM_ (\c -> putStrLn $ "  " ++ pretty c) cs
                    liftIO $ putStrLn "#"
                    

data OStack = OStack { -- ordered stack
      oAddToTop     :: [Card] -> Game (),  -- top is low
      oAddToBottom  :: [Card] -> Game (),
      oGetStack     :: Int -> Game [Card], -- top is front
      oStackName    :: StackName
    }

orderedStack :: StackName -> OStack
orderedStack sn = OStack { oAddToTop = att, oAddToBottom = atb,
                           oGetStack = gs, oStackName = sn }
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
      uGetStack    :: Int -> Game [Card],
      uStackName    :: StackName
    }

unorderedStack :: StackName -> UStack
unorderedStack sn = UStack { uAddToStack = add, uGetStack = gs,
                             uStackName = sn }
    where add cs = do let upd c = (cardId c, (sn,fromIntegral $ cardId c,c))
                          updates = map upd cs
                      modify $ \s -> s { gameCards = gameCards s//updates }
          gs _ = (map snd . concatMap issn . elems) `fmap` gets gameCards
          issn (st,x,c) = if st == sn
                          then [(x,c)]
                          else []

class NamedStack s where
    stackName :: s -> StackName
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
instance NamedStack OStack where
    stackName = oStackName
instance UnorderedInputStack UStack where
    addToStack = uAddToStack
instance OutputStack UStack where
    unorderedGetStack = uGetStack
instance NamedStack UStack where
    stackName = uStackName

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

infixr 1 *<<, .<<, <<
(*<<), (.<<) :: OrderedInputStack s => s -> [Card] -> Game ()
(*<<) = addToTop
(.<<) = addToBottom
(<<) :: UnorderedInputStack s => s -> [Card] -> Game ()
(<<)  = addToStack

-- These are to be combine with (<*) and (<.) to make e.g. *<#5<*
-- same precedence as =<<
infixr 1 *<#, .<#, <#
(*<#), (.<#) :: OrderedInputStack s => s -> Game [Card] -> Game ()
(*<#) = (=<<) . (*<<)
(.<#) = (=<<) . (.<<)
(<#) :: UnorderedInputStack s => s -> Game [Card] -> Game ()
(<#)  = (=<<) . (<<)

-- This is silly too - these are subtly different from =<< in
-- that the function on the lhs returns (), rather than the
-- result, and then we thread the argument back through.
-- If we had a utility function thread :: (a->m b) -> (a->m a)
-- then f #< a = (thread f) =<< a
infixr 1 #<, #<#
(#<) :: ([Card] -> Game ()) -> [Card] -> Game [Card]
f #< cs = f cs >> return cs

(#<#) :: ([Card] -> Game ()) -> Game [Card] -> Game [Card]
(#<#) = (=<<) . (#<)

top, bottom :: OrderedOutputStack s => Int -> s -> Game [Card]
top n s = take n `fmap` orderedGetStack s n
bottom n s = (take n . reverse) `fmap` orderedGetStack s n

infixr 1 <., <*
(<.), (<*) :: OrderedOutputStack s => Int -> s -> Game [Card]
(<.) = bottom
(<*) = top

infix 1 .<<<, *<<<, <<<
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
deck p = OStack att atb gs sn
    where OStack att atb gs0 sn = orderedStack (SPId p "deck")
          gs n = do x <- gs0 undefined {- ignored -}
                    if length x >= n
                       then return x
                       else do d <- get 0 $ discard p
                               d' <- shuffle d
                               atb d'
                               return $ x++d'

discard :: PId -> OStack
discard p = orderedStack $ SPId p "discard"

mat :: String -> PId -> OStack
mat n p = orderedStack $ SPId p $ "stack-"++n

durations :: PId -> UStack
durations p = unorderedStack $ SPId p "durations"

played :: UStack
played = unorderedStack $ SN "turnPlayed"

prevDuration :: UStack
prevDuration = unorderedStack $ SN "prev-duration"

aside :: UStack
aside = unorderedStack $ SN "aside"

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
||| Merge spurious conflicts. >>>

<<< Merge spurious conflicts. |||
||| resolve conflicts >>>

cardWhere :: Card -> Game StackName
cardWhere c = do cs <- gets gameCards
                 let (s,_,_) = cs ! cardId c
                 return s

<<< resolve conflicts |||
