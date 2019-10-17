module Main where

  import Network.Socket
  import System.IO
  import System.Directory
  import Control.Exception
  import Control.Concurrent
  import Control.Monad (when)
  import Control.Monad.Fix (fix)
  import ChatClassifier
  import BayesClassifier
  
  main :: IO ()
  main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4241 iNADDR_ANY)
    listen sock 2
    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
      (_, _) <- readChan chan
      loop
    mainLoop sock chan 0
  
  type Msg = (Int, String)
  
  mainLoop :: Socket -> Chan Msg -> Int -> IO ()
  mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum)
    mainLoop sock chan $! msgNum + 1
  
  runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
  runConn (sock, _) chan msgNum = do
      let broadcast msg = writeChan chan (msgNum, msg)
      hdl <- socketToHandle sock ReadWriteMode
      curdDir <- getCurrentDirectory
      chat_classifier <- trainedClassifier (curdDir ++ "/project/src/questions.csv")
      hSetBuffering hdl NoBuffering

      -- Initial Message
      hPutStrLn hdl "Chatbot: Hello, what did you need assistance with?"

      -- Message Loop
      handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
          question <- fmap init (hGetLine hdl)
          case question of
               -- If an exception is caught, send a message and break the loop
               "quit" -> hPutStrLn hdl "Chatbot: Goodbye!"
               -- else, continue looping.
               _ -> do
                  hPutStrLn hdl ("User: " ++ question)

                  -- get answer
                  let category = classify chat_classifier question
                  hPutStrLn hdl ("Category: " ++ category)

                  let prob = probabilityForCategory chat_classifier question category
                  let ansDir = (curdDir ++ "/project/src/answers.csv")
                  str <- fetchAnswer ansDir (dropWhile (==' ') category)

                  hPutStrLn hdl ("Prob: " ++ show prob)
                  hPutStrLn hdl ("Chatbot: " ++ str)

                  hPutStrLn hdl "Chatbot: To help us improve, was this answer helpful? (y/n)"
                  answer <- fmap init (hGetLine hdl)
                  if answer == "y"
                    then 
                      --- if answer is good, add question with category to questions.csv
                      hPutStrLn hdl "Chatbot: If you need anymore help feel free to ask another question, otherwise type 'quit' to exit"
                    else
                      -- nothing
                      hPutStrLn hdl "Chatbot: Sorry, please ask the question again for a more relevant answer"

                  loop
      hClose hdl                             -- close the handle