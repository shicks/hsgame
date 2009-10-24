#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $ do autoVersion Numbered
                     ghcFlags ["-threaded", "-Wall",
                               "-funbox-strict-fields","-O2"]
                     package "yaml-simple" ["YAML"] []
