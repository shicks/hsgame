module Dominion ( ) where

data CardData = CardData String Int String -- picture...?

data Pile = Pile String -- just name w/ strings for now - heirarchy later

data Question = PickOption [(Int,String)]
              | PickCardsFromPile Pile      -- ^ which pile
                                  (Int,Int) -- ^ min/max
                                  [Card]    -- ^ allowed cards

data DominionMessage = MsgCardData CardData
                     | MsgPileContents Pile [Card]
                     | MsgCardMovement Card Pile Pile
                     | MsgQuestion String Question

data DominionResponse = PickedOption Int
                      | PickedCards [Card]

