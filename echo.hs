import TCP.Server ( startServer )
import Control.Monad ( forever )
import System.IO ( hPutStrLn, hGetLine )

main = startServer 12345 $ \n h -> forever $ hGetLine h >>= hPutStrLn h
