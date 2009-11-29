||| Merge >>>
module Dominion.Unique ( newQId, newCId ) where

import Dominion.Types ( Game(_qids,_cids), QId, CId, Card(..) )

newQId :: Game QId
newQId = do qs <- gets _qIds
            modify $ \s -> { _qIds = tail qs }
            return head qs

newCId :: Game CId
newCId = do cs <- gets _cIds
            modify $ \s -> { _cIds = tail cs }
            return head cs

copyCard :: Card -> Game Card
copyCard c = do u <- newCId
                return $ c { cardId = u }

<<< Merge |||
||| Merge stupidly? >>>
module Dominion.Unique ( newQId, newCId, copyCard ) where

import Dominion.Types ( GameState(_qIds,_cIds), Game, QId, CId, Card(..) )

import Control.Monad.State ( modify, gets )

newQId :: Game QId
newQId = do qs <- gets _qIds
            modify $ \s -> s { _qIds = tail qs }
            return $ head qs

newCId :: Game CId
newCId = do cs <- gets _cIds
            modify $ \s -> s { _cIds = tail cs }
            return $ head cs

copyCard :: Card -> Game Card
copyCard c = do u <- newCId
                return $ c { cardId = u }

<<< Merge stupidly? |||
