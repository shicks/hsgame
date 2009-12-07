module HTTP.FileServer ( fileServer ) where

import HTTP.Request ( Request(..), Method(..) )
import HTTP.Response ( Response, blank200, ok200,
                       error400, error403, error404, error501 )

import Data.Char ( toLower )
import Data.List ( isPrefixOf )
import Network.URI ( URI(..) )
import System.Directory ( canonicalizePath, makeRelativeToCurrentDirectory,
                          doesFileExist, doesDirectoryExist )
import System.FilePath ( (</>), takeExtension )

contentType :: String -> String
contentType s | map toLower s `elem` [".jpg",".jpeg"] = "image/jpeg"
              | map toLower s == ".png" = "image/png"
              | map toLower s == ".gif" = "image/gif"
              | map toLower s `elem` [".htm",".html"] = "text/html"
              | map toLower s == ".css" = "text/html"
              | map toLower s == ".js" = "text/javascript"
              | map toLower s == ".xml" = "text/xml"
              | otherwise = "text/plain"

-- We need to take some care here that we only send files in the
-- root directory and lower.  We'll take that dir to be a compile-time
-- constant, "./htdocs/" - symlinks won't even be allowed for now...
-- Maybe later we'll find some way to add "allowed dirs", but for
-- now we'll err on the side of security.
sendFile :: String -> IO Response
sendFile f = do --putStrLn $ "sendFile "++f
                cf <- canonicalizePath $ "htdocs" </> dropWhile (=='/') f
                --putStrLn $ "  cf = "++cf
                rf <- makeRelativeToCurrentDirectory cf
                --putStrLn $ "  rf = "++rf
                if "htdocs" `isPrefixOf` rf
                   then send' rf
                   else error403
    where send' rf = do de <- doesDirectoryExist rf
                        --putStrLn $ "send' "++rf++": de="++show de
                        if de then do
                                let ix = rf </> "index.html"
                                ie <- doesFileExist $ ix
                                if ie then send'' ix
                                      else error403 -- maybe sendDir rf
                              else do
                                fe <- doesFileExist rf
                                if fe then send'' rf else error404
          send'' rf = do s <- readFile rf
                         let ext = takeExtension rf
                             ctype = contentType ext
                         ok200 ctype s

-- PureServer
fileServer :: Request -> IO Response          -- serves files
fileServer req = do case rqMethod req of
                      NotImplemented -> error501
                      OPTIONS -> blank200
                      _ -> case muri of
                             Nothing  -> error400
                             Just uri -> sendURI uri
    where muri = rqURI req
          sendURI uri = sendFile $ uriPath uri
          -- pattern-match on (`isPrefixOf` uriPath uri)
