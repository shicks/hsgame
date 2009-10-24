#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $ do -- autoVersion Numbered
                     version "0.0"
                     checkOnce "if we accept PostfixOperators" $
                               do ghcFlags ["-XPostfixOperators"]
                                  requireModule "Prelude"
                     ghcFlags ["-threaded", "-Wall",
                               "-funbox-strict-fields","-O2"]
                     package "yaml-simple" ["YAML"] []
