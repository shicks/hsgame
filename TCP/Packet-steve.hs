module TCP.Packet ( Packet, 

                  ) where

import YAML ( YAML(..), parseYaml, singleT )
import Data.Maybe ( fromJust )

data Packet a b = Packet { from    :: Address a
                           to      :: Address a
                           payload :: b }

data Address a = Address a (Node -> IO ())

mkPacket :: YAML b => a -> (b -> IO ()) -> Packet a
mkPacket a next = Packet a $ next . fromJust . fromNode

data LobbyTarget = LobbyTable Int | LobbyUser String | LobbyChat | LobbyJoin
instance YAML LobbyTarget where
    fromNode (LobbyTable t) = singleT (Leaf "table") (Leaf $ show t)
    fromNode (LobbyUser u) = singleT (Leaf "user") (Leaf u)
    fromNode LobbyChat = Leaf "chat"
    fromNode LobbyJoin = Leaf "join"
    toNode n | Just t <- fromNode `fmap` getMapping "table" n
                      = Just $ LobbyTable t
             | Just u <- getScalar `fmap` getMapping "user" n
                      = Just $ LobbyUser u
             | otherwise = case getScalar n of
                             Just "chat" -> Just LobbyChat
                             Just "join" -> Just LobbyJoin
                             _ -> Nothing

data TableTarget = TableUser String | TableChat | TableJoin

-- Let's say we want to address a packet to Lobby:Table 4:User figaro:PrivMsg
-- Then we'll

data UserTarget = PrivMsg

lobbyRouting :: LobbyTarget -> Node -> IO ()
lobbyRouting (LobbyTable t) = routeWith tableRouting
lobbyRouting (LobbyUser u)  = routeWith userRouting
lobbyRouting LobbyChat      = doChat
lobbyRouting LobbyJoin      = doJoin

routeWith :: (Address a -> IO ()) -> Node -> IO ()
routeWith routingTable node = 