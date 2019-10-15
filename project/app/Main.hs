-- Main.hs, final code
module Main where

    import Network.Socket
    import System.IO
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
      bind sock (SockAddrInet 4242 iNADDR_ANY)
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
        chat_classifier <- trainedClassifier "./src/chatbot-training-data.csv"
        hSetBuffering hdl NoBuffering
        


interactionLoop chat_classifier probabilities = do
  hPutStrLn hdl "What do you need assistance with?"
  question <- fmap init (hGetLine hdl)
  let category = classify chat_classifier question
  -- get top response for category
  -- hPutStrLn hdl (getTopResponse probabilities category)

  hPutStrLn hdl "Was this what you were looking for? (y/n)"
  answer <- fmap init (hGetLine hdl)
  if answer == "y"
    then 
      -- interactionloop chat_classifier (bumpProbability category response)
      -- increase choice probability ( if not top choice)
    else
      -- show next answer
      -- hPutStrLn hdl (classify chat_classifier text)

  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $ fix $ \loop -> do
      (nextNum, line) <- readChan commLine
      when (msgNum /= nextNum) $ hPutStrLn hdl line
      loop

  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
      line <- fmap init (hGetLine hdl)
      case line of
           -- If an exception is caught, send a message and break the loop
           "quit" -> hPutStrLn hdl "Bye!"
           -- else, continue looping.
           --_      -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader                      -- kill after the loop ends
  --broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
  hClose hdl                             -- close the handle