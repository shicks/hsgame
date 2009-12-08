module HTTP.Response ( Response(..), ok200,
                       continue100, blank200, blank204, redirect303,
                       error400, error403, error404, error405, error418,
                       error500, error501, error503,
                       sendResponse, sendResponseHdr,
                       htmlResponse, htmlResponse', jsResponse,
                       sanitize, jsPrintf
                     ) where

import System.IO ( hPutStrLn, hPutStr, Handle )
import Network.HTTP.Headers ( Header(..), HeaderName(..), HasHeaders(..) )

import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime )
import System.Locale ( defaultTimeLocale )

data Response = Response {
      respVersion :: String,
      rspCode :: Int,
      rspReason :: String,
      rspHeaders :: [Header],
      rspBody :: String
} deriving ( Show )

instance HasHeaders Response where
    getHeaders (Response _ _ _ hs _) = hs
    setHeaders r hs = r { rspHeaders = hs }

-- |This is the main response-generating function
ok200 :: String -> String -> IO Response
ok200 ctype body = do t <- date
                      return $ Response "HTTP/1.1" 200 "OK" (hs t) body
    where hs t = (m HdrContentType ctype):
                 (m HdrContentLength $ show $ length body):
                 (m HdrDate t):[]
          m = Header

-- |All sorts of other responses...
continue100 :: IO Response
continue100 = do t <- date
                 return $ Response "HTTP/1.1" 101 "Continue"
                            [Header HdrDate t] ""

blank200 :: IO Response
blank200 = do t <- date
              return $ Response "HTTP/1.1" 200 "OK"
                         [Header HdrDate t,Header HdrContentLength "0"] ""

blank204 :: IO Response
blank204 = do t <- date
              return $ Response "HTTP/1.1" 204 "No Content"
                         [Header HdrDate t,Header HdrContentLength "0"] ""

redirect303 :: String -> IO Response
redirect303 l = htmlResponse' 303 "See Other" "HTTP/1.1" [loc] $ unlines
                ["<html><head><title>303 See Other</title>",
                 "</head><body><h1>See Other</h1>",
                 "<p>The resource you requested is located at",
                 "<a href="++l++">"++l++"</a>.  You should be",
                 "redirected there momentarily.</p>",
                 "</body></html>"]
    where loc = Header HdrLocation l

error400 :: IO Response
error400 = htmlResponse' 400 "Bad Request" "HTTP/1.1" [] $ unlines
           ["<html><head><title>400 Bad Request</title>",
            "</head><body><h1>Bad Request</h1>",
            "<p>Your client has issued a malformed or illegal request.</p>",
            "</body></html>"]

error403 :: IO Response
error403 = htmlResponse' 403 "Forbidden" "HTTP/1.1" [] $ unlines
           ["<html><head><title>403 Forbidden</title>",
            "</head><body><h1>Forbidden</h1>",
            "<p>You do not have permission to access this resource.</p>",
            "</body></html>"]

error404 :: IO Response
error404 = htmlResponse' 404 "Not Found" "HTTP/1.1" [] $ unlines
           ["<html><head><title>404 Not Found</title>",
            "</head><body><h1>Not Found</h1>",
            "<p>The resource you requested could not be found.</p>",
            "</body></html>"]

error405 :: IO Response
error405 = htmlResponse' 405 "Method Not Allowed" "HTTP/1.1" [] $ unlines
           ["<html><head><title>405 Method Not Allowed</title>",
            "</head><body><h1>Method Not Allowed</h1>",
            "<p>This resource does not allow the method you're using.</p>",
            "</body></html>"]

error418 :: IO Response
error418 = htmlResponse' 418 "I'm a teapot" "HTTP/1.1" [] $ unlines
           ["<html><head><title>418 I'm a teapot</title>",
            "</head><body><h1>I'm a teapot</h1>",
            "<p>I'm a little teapot, short and stout",
            "<br/>Here is my handle, here is my spout",
            "<br/>When I get excited, watch me shout",
            "<br/>Just tip me over and pour me out.</p>",
            "</body></html>"]

error500 :: IO Response
error500 = htmlResponse' 500 "Internal Server Error" "HTTP/1.1" [] $ unlines
           ["<html><head><title>500 Internal Server Error</title>",
            "</head><body><h1>Internal Server Error</h1>",
            "<p>Something went wrong.</p>",
            "</body></html>"]

error501 :: IO Response
error501 = htmlResponse' 501 "Not Implemented" "HTTP/1.1" [] $ unlines
           ["<html><head><title>501 Not Implemented</title>",
            "</head><body><h1>Not Implemented</h1>",
            "<p>This server doesn't support that method.</p>",
            "</body></html>"]

error503 :: IO Response
error503 = htmlResponse' 503 "Service Unavailable" "HTTP/1.1" [] $ unlines
           ["<html><head><title>503 Service Unavailable</title>",
            "</head><body><h1>Service Unavailable</h1>",
            "<p>The service you requested is currently unavailable. ",
            "Please try again later.</p>",
            "</body></html>"]

date :: IO String
date = formatTime defaultTimeLocale "%a, %d %b %Y %X %Z" `fmap` getCurrentTime

sendResponse :: Handle -> Response -> IO ()
sendResponse h (Response v c r hs b) = do hPutStrLn h $ v++" "++show c++" "++r
                                          mapM_ (hPutStr h . show) hs
                                          hPutStrLn h ""
                                          hPutStrLn h b
                                          putStrLn $ "Response "++show c++" "
                                                       ++take 60 (show b)

-- |Sends just the code and headers - useful for streaming?!?
sendResponseHdr :: Handle -> Response -> IO ()
sendResponseHdr h (Response v c r hs _) =
    do hPutStrLn h $ v++" "++show c++" "++r
       mapM_ (hPutStr h . show) hs
       hPutStrLn h ""

htmlResponse' :: Int -> String -> String -> [Header] -> String -> IO Response
htmlResponse' c r v hs0 b = do t <- date
                               return $ Response v c r (hs t) b
    where hs t = (m HdrContentType "text/html; charset=UTF-8"):
                 (m HdrContentLength $ show $ length b):
                 (m HdrDate t):hs0
          m = Header

htmlResponse :: String -> IO Response
htmlResponse = ok200 "text/html; charset=UTF-8"

jsResponse :: String -> IO Response
jsResponse = ok200 "text/javascript; charset=UTF-8"

-- these are useful for making responses...
sanitize :: String -> String
sanitize [] = []
sanitize ('\\':s) = '\\':'\\':sanitize s
sanitize ('"':s) = '\\':'"':sanitize s
sanitize (s:ss) = s:sanitize ss

jsPrintf :: String -> [String] -> String
jsPrintf "" _ = ""
jsPrintf ('%':'s':cs) (s:ss) = '"':sanitize s++'"':jsPrintf cs ss
jsPrintf ('%':'s':cs) [] = '"':'"':jsPrintf cs [] -- or fail?!?
jsPrintf (c:cs) ss = c:jsPrintf cs ss
