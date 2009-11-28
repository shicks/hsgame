import TCP.Server ( startServer )
import Control.Monad ( forever )
import System.IO ( hPutStrLn )

main = startServer 12345 $ \n h -> forever $ hPutStrLn h $ "hello " ++ n
