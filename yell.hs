import TCP.Server ( startServer )
import TCP.Chan ( writeOutput )

import Control.Monad ( forever )

main :: IO ()
main = startServer 12345 $ \n _ o ->
       forever $ writeOutput o ("hello " ++ n)
