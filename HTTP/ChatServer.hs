module HTTP.ChatServer ( chatServer, chatThread ) where

import HTTP.Request ( Request(..), urlDecode, )
import HTTP.Response ( Response, error403, jsResponse, blank200 )

import Control.Concurrent ( forkIO, threadDelay )
import TCP.Chan ( Input, Output, pipe, isEmptyInput,
                  writeOutput, readInput )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )
import Data.Maybe ( fromMaybe )
import Network.URI ( URI(..), URIAuth(..) )

authServer :: (String -> String -> String -> Request -> IO Response)
           -> Request -> IO Response
authServer srv req = do case rqURI req of
                          Nothing -> srv "" "*" "" req
                          Just uri -> srv (user uri) (uriPath uri)
                                          (query uri) req
    where user' uri = (takeWhile (not . (`elem`":@")) .
                       uriUserInfo) `fmap` uriAuthority uri
          user uri = fromMaybe "" $ user' uri
          query uri = dropWhile (=='?') $ uriQuery uri

data Chat = NC String | C String | P String (MVar Response)

chatServer :: Output Chat -> (Request -> IO Response) -> Request -> IO Response
chatServer ct srv = authServer go
    where go _ "/chat/login" q _ =
              do let u = fromMaybe "anon" $ lookup "u" $ urlDecode q
                 writeOutput ct $ NC u
                 writeOutput ct $ C $ "Welcome, " ++ u
                 jsResponse "$(\"#chatarea\").show();\n"
                       -- might need to change $("#name").val()
          go _ "/chat/say" q _ =
              do let u = fromMaybe "anon" $ lookup "u" $ urlDecode q
                 writeOutput ct $ C (u++": "++text)
                 blank200
            where text = fromMaybe "" $ lookup "q" $ urlDecode q
          go _ "/chat/poll" q _ =
              do let u = fromMaybe "anon" $ lookup "u" $ urlDecode q
                 rsp <- newEmptyMVar
                 writeOutput ct $ P u rsp
                 takeMVar rsp
          go _ _ _ r = srv r -- pass on to next server

chatThread :: Input Chat -> [(String,(Input String,Output String))] -> IO ()
chatThread inp ag = do msg <- readInput inp
                       case msg of
                         NC name -> do chans <- pipe
                                       chatThread inp ((name,chans):ag)
                         C s -> do mapM_ (\(_,(_,o)) -> writeOutput o s) ag
                                   chatThread inp ag
                         P name mv -> do forkIO $ do
                                           let mc = lookup name ag
                                           case mc of
                                             Nothing -> do
                                                     threadDelay 5000000
                                                     putMVar mv =<< error403
                                             Just (c,_) -> emptyChan c mv
                                         chatThread inp ag
    where emptyChan c mv = do line <- readInput c
                              rest <- ec' c
                              putMVar mv =<< jsResponse
                                             (unlines $ map say (line:rest))
          ec' c = do e <- isEmptyInput c
                     if e then return [] else do l <- readInput c
                                                 (l:) `fmap` ec' c
          say s = "$(\"#chat\").append(\""++sanitize s++"\\n\");\n"
                  ++ "$(\"#chat\").scrollTop(1e10);\n"
          sanitize [] = []
          sanitize ('\\':s) = '\\':'\\':sanitize s
          sanitize ('"':s) = '\\':'"':sanitize s
          sanitize (s:ss) = s:sanitize ss
