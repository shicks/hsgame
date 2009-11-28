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
