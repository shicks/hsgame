module Main where

import YAML ( parseYamlList )

main :: IO ()
main = do let partialstring = unlines ["- foo","- bar",undefined]
          putStrLn "The partialstring has two defined elements:"
          print $ take 2 $ lines partialstring
          putStrLn "parseYamlList should find those two elements:"
          print $ take 2 $ parseYamlList partialstring
          putStrLn "You may now play with parseYamlList interactively:"
          x <- parseYamlList `fmap` getContents
          mapM_ print x
