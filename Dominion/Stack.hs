{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dominion.Stack ( Deck, UStackName, IStack, printStack,
                        addCards, initializeStackHooks,
                        (*<<), (.<<), (<<), (*<#), (.<#), (<#),
                        (<*), (<.), (*<<<), (.<<<), (<<<), (#<), (#<#),
                        stackName, getStack, top, bottom,
                        shuffle, shuffleIO, supply,
                        draw, hand, deck, discard, mat, trash,
                        durations, played, prevDuration, aside,
                        allSupply, supplyCards, inSupply, inTrash,
                        allCards,
                        gain, gain', gainSilent,
                        revealHand, revealCards,
                        cardWhere ) where

import Dominion.Types
import Dominion.Message

import Control.Monad ( forM, forM_, when, filterM )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Random ( randomRIO )
import Data.Array ( elems, listArray, bounds, (//), (!) )
import Data.List ( sortBy, groupBy )
import Data.Ord ( comparing )
import Data.Function ( on )

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

instance NamedStack StackName where
    stackName = id
instance OutputStack StackName where
    unorderedGetStack sn _ =
        (map snd . sortBy (comparing fst) . concatMap issn . elems)
        `fmap` gets gameCards
        where issn (st,x,c) = if st == sn then [(x,c)] else []
instance OrderedOutputStack StackName where
    orderedGetStack = unorderedGetStack
instance OrderedInputStack StackName where
    addToTop sn cs =
        do hook <- lookup sn `fmap` gets stackHooks
           new <- maybe (return cs) ($ cs) hook
           old <- orderedGetStack sn 0
           let upd depth c = (cardId c, (sn,depth,c))
               updates = zipWith upd [1..] (reverse new++old)
           modify $ \s -> s { gameCards = gameCards s//updates}
    addToBottom sn cs =
        do hook <- lookup sn `fmap` gets stackHooks
           new <- maybe (return cs) ($ cs) hook
           old <- orderedGetStack sn 0
           let upd depth c = (cardId c, (sn,depth,c))
               updates = zipWith upd [1..] (old++new)
           modify $ \s -> s { gameCards = gameCards s//updates }

newtype UStackName = USN StackName
instance NamedStack UStackName where
    stackName (USN x) = x
instance OutputStack UStackName where
    unorderedGetStack (USN sn) n = unorderedGetStack sn n
instance UnorderedInputStack UStackName where
    addToStack (USN sn) = addToTop sn

get :: OutputStack s => Int -> s -> Game [Card]
get = flip unorderedGetStack

data IStack s = IStack {
      inputProc :: [Card] -> Game [Card],
      origStack :: s
}
instance OrderedInputStack s => OrderedInputStack (IStack s) where
    addToBottom s cs = inputProc s cs >>= addToBottom (origStack s)
    addToTop s cs = inputProc s cs >>= addToTop (origStack s)
instance UnorderedInputStack s => UnorderedInputStack (IStack s) where
    addToStack s cs = inputProc s cs >>= addToStack (origStack s)

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
          (_:xs) !- 0 = xs
          (x:xs) !- n = x:(xs!-(n-1))

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
draw n p = do cs <-n<* deck p
              hand p << cs
              name <- withPlayer p $ gets playerName
              tellP p $ CardDraw name $ Right $ map describeCard cs
              tellOppOf p $ CardDraw name $ Left $ length cs

hand :: PId -> UStackName
hand p = USN $ SPId p "hand"

-- we've built in the reshuffling mechanism here...!
newtype Deck = Deck PId
instance OrderedInputStack Deck where
    addToBottom = addToBottom . stackName
    addToTop = addToTop . stackName
instance OrderedOutputStack Deck where
    orderedGetStack d@(Deck p) n =
        do x <- orderedGetStack (stackName d) n
           if length x >= n
               then return x
               else do cs <- get 0 $ discard p
                       cs' <- shuffle cs
                       addToBottom d cs'
                       name <- withPlayer p $ gets playerName
                       tellAll $ Reshuffled name
                       return $ x++cs'
instance OutputStack Deck where
    unorderedGetStack = orderedGetStack
instance NamedStack Deck where
    stackName (Deck p) = SPId p "deck"

deck :: PId -> Deck
deck = Deck

initializeStackHooks :: [PId] -> Game ()
initializeStackHooks pids = modify $ \s -> s { stackHooks = hs ++ stackHooks s }
    where hs = playhook:(stackName trash, trashHook):map discardHook pids
          discardHook pid = (discard pid, dh pid)
          dh pid cs = do cs' <- filterM from cs
                         name <- withPlayer pid $ gets playerName
                         when (not $ null cs') $
                              tellAll $ CardDiscard name $ map describeCard cs'
                         return cs
          from c = do loc <- cardWhere c
                      case loc of
                        SN "aside" -> return True
                        SPId _ "hand" -> return True
                        _ -> return False
          playhook = (stackName played, ph)
              where ph cs =
                        do n <- getSelf >>= withPlayer `flip` gets playerName
                           tellAll $ CardPlay n $ map describeCard cs
                           return cs
          -- this is complicated because we need to figure out who
          -- OWNED the card before, and we'd like to group it into a
          -- single message, if possible, for each owner.
          trashHook cs =
              do cs' <- forM cs $ \c ->
                        do loc <- cardWhere c
                           owner <- case loc of
                                      SPId p _ -> withPlayer p $ gets playerName
                                      SN "played" -> do p <- getSelf
                                                        withPlayer p $
                                                            gets playerName
                                      _ -> return "Somebody"
                           return (owner,c)
                 let srt = groupBy ((==) `on` fst) $ sortBy (comparing fst) cs'
                 forM_ srt $ \ss ->
                     do let p = fst $ head ss
                            ss' = map (describeCard.snd) ss
                        tellAll $ CardTrash p ss'
                 return cs

discard :: PId -> StackName
discard p = SPId p "discard"

mat :: String -> PId -> StackName
mat n p = SPId p $ "stack-"++n

durations :: PId -> UStackName
durations p = USN $ SPId p "durations"

played :: UStackName
played = USN $ SN "played"

prevDuration :: UStackName
prevDuration = USN $ SN "prev-duration"

aside :: UStackName
aside = USN $ SN "aside"

trash :: UStackName
trash = USN $ SN "trash"

supply :: UStackName -- messages?
supply = USN $ SN "supply"

allSupply :: Game [Card] -- rewrite in terms of supply?
allSupply = (concatMap iss . elems) `fmap` gets gameCards
    where iss (SN "supply",_,c) = [c]
          iss _ = []

supplyCards :: Card -> Game [Card]
supplyCards c = filter (sameName c) `fmap` allSupply

inSupply :: Card -> Game Bool
inSupply c = (not . null) `fmap` supplyCards c

inTrash :: Card -> Game Bool
inTrash c = (== stackName trash) `fmap` cardWhere c

-- *this is a funny distinction.  @gain@ will be used only for
-- gaining cards from the supply, while @gain'@ will be used for
-- gaining a particular instance of a card.  If a card is not
-- in the supply then @gain@ will fail.  'hookGain' is run in
-- both cases.  Note that failure means that smugglers must either
-- use 'try' or else filter out unavailable cards.
gain :: PId -> (PId -> s) -> IStack s
gain p s = IStack gain'' (s p)
    where gain'' cs = do cs' <- concat `fmap` mapM checkSupply cs
                         runGainHooks p cs'
                         name <- withPlayer p $ gets playerName
                         when (not $ null cs') $
                              tellAll $ CardGain name $ map describeCard cs'
                         return cs'
          checkSupply c = do cs <- take 1 `fmap` supplyCards c
                             aside << cs  -- workaround treasureMap issue
                             return cs

gain' :: PId -> (PId -> s) -> IStack s
gain' p s = IStack (thread gain'') (s p)
    where gain'' cs = do runGainHooks p cs
                         name <- withPlayer p $ gets playerName
                         when (not $ null cs) $
                              tellAll $ CardGain name $ map describeCard cs

-- no announcement because we already announced Buy...
gainSilent :: PId -> (PId -> s) -> IStack s
gainSilent p s = IStack gain'' (s p)
    where gain'' cs = mapM checkSupply cs >>= thread (runGainHooks p) . concat
          checkSupply c = do cs <- take 1 `fmap` supplyCards c
                             aside << cs
                             return cs

allCards :: PId -> Game [Card]
allCards p = (concatMap isp . elems) `fmap` gets gameCards
    where isp (st,_,c) = case st of
                           SPId p' _ | p' == p -> [c]
                           _ -> []

cardWhere :: Card -> Game StackName
cardWhere c = do cs <- gets gameCards
                 let (s,_,_) = cs ! cardId c
                 return s

thread :: Monad m => (a -> m b) -> a -> m a
thread f a = f a >> return a


revealHand :: PId -> Game ()
revealHand p = do h <- getStack $ hand p
                  name <- withPlayer p $ gets playerName
                  tellAll $ CardReveal name (map describeCard h) "hand"

revealCards :: PId -> [Card] -> String -> Game ()
revealCards p cs f = do name <- withPlayer p $ gets playerName
                        tellAll $ CardReveal name (map describeCard cs) f
