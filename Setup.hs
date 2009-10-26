#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $ do -- autoVersion Numbered
                     version "0.0"
                     checkOnce "if we accept PostfixOperators" $
                               do ghcFlags ["-XPostfixOperators"]
                                  requireModule "Data.List"
                     ghcFlags ["-threaded", -- "-Wall",
                               "-funbox-strict-fields","-O2"]
                     package "yaml-simple" ["YAML"] []
                     executable "yell" "yell.hs" []
                     executable "echo" "echo.hs" []
                     executable "testyaml" "testyaml.hs" []
                     executable "telnet" "telnet.hs" []
                     executable "chat" "chat.hs" []
                     executable "purechat" "purechat.hs" []
                     executable "named-chat" "named-chat.hs" []
                     executable "yamltest" "yamltest.hs" []
