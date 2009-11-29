||| Merge >>>
import YAML

import System.IO

main = do hSetBuffering stdin NoBuffering
          getContents >>= mapM_ print . makeTokens

<<< Merge |||
||| Merge stupidly? >>>
import YAML

import System.IO

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          getContents >>= mapM_ print . makeTokens

<<< Merge stupidly? |||
