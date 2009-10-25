import TCP.Server ( startServer )

import Control.Concurrent ( Chan, writeChan )
import Control.Monad ( forever )

main :: IO ()
main = startServer 12345 $ \n i o ->
       forever $ seq (i :: Chan String) $ writeChan o ("hello " ++ n)
