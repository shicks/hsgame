module Main where

import YAML ( parseYamlList, parseYaml, makeTree, makeTokens )

main :: IO ()
main = do let partialstring = unlines ["- foo","- bar" --, undefined
                                      ]
          putStrLn "The partialstring has two defined elements:"
          print $ take 2 $ lines partialstring
          putStrLn "makeTokens should find those two elements:"
          mapM_ print $ makeTokens partialstring
          putStrLn "makeTree should find those two elements:"
          mapM_ print $ makeTree partialstring
          putStrLn "parseYamlList should find those two elements:"
          mapM_ print $ parseYaml partialstring
          print $ take 1 $ parseYamlList partialstring
          putStrLn "You may now play with parseYamlList interactively:"
          x <- parseYamlList `fmap` getContents
          mapM_ print x
