import YAML

import System.IO

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          getContents >>= mapM_ print . makeTokens
