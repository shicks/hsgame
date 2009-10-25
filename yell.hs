import TCP.Server ( startServer )
import TCP.Chan ( Input, writeOutput )

import Control.Monad ( forever )

main :: IO ()
main = startServer 12345 $ \n i o ->
       forever $ seq (i :: Input String) $ writeOutput o ("hello " ++ n)
