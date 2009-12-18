#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $ do -- autoVersion Numbered
                     version "0.0"
                     checkOnce "if we accept PostfixOperators" $
                               do ghcFlags ["-XPostfixOperators"]
                                  requireModule "Data.List"
                     ghcFlags ["-threaded", "-Wall", "-Werror",
                               "-funbox-strict-fields","-O2"]
                     executable "named-chat" "named-chat.hs" []
                     executable "testdom" "testdom.hs" []
                     executable "lobby-chat" "lobby-chat.hs" []
                     executable "httpd" "httpd.hs" []
