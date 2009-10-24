#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $ do autoVersion Numbered
                     checkOnce "if we accept PostfixOperators" $
                               do ghcFlags ["-XPostfixOperators"]
                                  requireModule "Prelude"
                     ghcFlags ["-threaded", "-Wall",
                               "-funbox-strict-fields","-O2"]
                     package "yaml-simple" ["YAML"] []
