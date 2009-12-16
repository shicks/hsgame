module Dominion.Bots ( pickDecks ) where

import Dominion.Types ( Card, cardPrice )
import Dominion.Cards ( allRecommended, allDecks )
import Dominion.Stack ( shuffleIO )

import System.Random ( randomRIO )
import Data.List ( sortBy )
import Data.Ord ( comparing )

pickDecks :: [Card] -> IO [Card]
pickDecks cs = do let sets = filter ((==10).length.snd) allRecommended
                  r <- randomRIO (1,100)
                  decks <- if r > (10 :: Int) || not (null cs)
                           then (take 10 . (cs++)) `fmap` shuffleIO allDecks
                           else do (sn,d) <- head `fmap` shuffleIO sets
                                   putStrLn ("Using set "++sn)
                                   return d
                  return $ sortBy (comparing cardPrice) decks
