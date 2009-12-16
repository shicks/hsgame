{-# LANGUAGE FlexibleInstances #-}

module Dominion.Pretty ( Pretty(pretty) ) where

import Dominion.Types
import Data.List ( intercalate )

class Pretty p where
    pretty :: p -> String

instance Pretty CardDescription where
    pretty c = cname c++" ("++show (cprice c)++")" --  ["++ show (cid c)++"]"
instance Pretty [CardDescription] where
    pretty cs = intercalate ", " $ map pretty cs
instance Pretty InfoMessage where
    pretty (InfoMessage s) | 1==1 = s -- prevent overlapping match warning
    pretty (GameOver s) = s
    pretty (CardPlay p cs) = p ++ " played " ++ pretty cs
    pretty (CardDraw p (Left n)) = p ++ " drew " ++ show n ++ " cards"
    pretty (CardDraw _ (Right cs)) = "You drew " ++ pretty cs
    pretty (CardDiscard p cs) = p ++ " discarded " ++ pretty cs
    pretty (CardTrash p cs) = p ++ " trashed " ++ pretty cs
    pretty (CardReveal p cs f) = p ++ " revealed " ++ pretty cs ++ " from " ++ f
    pretty (CardBuy p cs) = p ++ " bought " ++ pretty cs
    pretty (CardGain p cs) = p ++ " gained " ++ pretty cs
    pretty (Reshuffled p) = p ++ " reshuffled"
    pretty s = show s -- prevent crashing when we add message types
instance Pretty Answer where
    pretty (Choose s) = s
    pretty (PickCard c) = pretty c
instance Pretty MessageToClient where
    pretty (Info i) = pretty i
    pretty (Question _ m as (a0,a1)) =
                unlines (["Question: "++show m,
                          "Options:"]++zipWith showopt [1..] as++
                         ["Enter "++showRange a0 a1++" separated by spaces: "])
            where showopt n a =
                      "  "++ show n++": "++pretty a
                  showRange 0 1 = "up to 1 number"
                  showRange 0 a = "up to " ++ show a ++ " numbers"
                  showRange 1 1 = "1 number"
                  showRange a b
                      | a==b = show a ++ " numbers"
                      | b==a+1 = show a ++ " or " ++ show b ++ " numbers"
                      | otherwise = " from " ++ show a ++ " to " ++ show a
                                    ++ " numbers"
