-- CODE REFERENCE: https://wiki.haskell.org/Implement_a_chat_server

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
  
  -- intialize socket and create channel for reading responses
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
  
  -- create a thread to read the responses
  mainLoop :: Socket -> Chan Msg -> Int -> IO ()
  mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum)
    mainLoop sock chan $! msgNum + 1
  
  -- run thread and listen for any response
  runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
  runConn (sock, _) chan msgNum = do
      let broadcast msg = writeChan chan (msgNum, msg)
      hdl <- socketToHandle sock ReadWriteMode
      curdDir <- getCurrentDirectory
      
      hSetBuffering hdl NoBuffering

      -- All code below is written by our group
      hPutStrLn hdl "Would you like to enable training mode (y/n)?"
      training <- fmap init (hGetLine hdl)

      -- Initial Message
      hPutStrLn hdl "Chatbot: Hello, what did you need assistance with?"

      -- Message Loop
      handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
          -- fetch question from command line
          question <- fmap init (hGetLine hdl)
          case question of
               -- If an exception is caught, send a message and break the loop
               "quit" -> hPutStrLn hdl "Chatbot: Goodbye!"
               -- else, continue looping.
               _ -> do
                  -- reload the classifier with updated training data
                  chat_classifier <- trainedClassifier (curdDir ++ "/project/src/questions.csv")
                  hPutStrLn hdl ("User: " ++ question)

                  -- clean input by removing connectives and fetch categories
                  let cleanq = cleanInput question 
                  cats <- readCsv (curdDir ++ "/project/src/categories.csv")      

                  -- check if question contains a reference to a category
                  let incat = compareList (cats !! 0) cleanq

                  -- if it does not contain a reference we can not give a valid answer so return and reloop]
                  -- otherwise get appropriate response
                  if (not incat)
                    then do
                      hPutStrLn hdl "Sorry, I can't answer that please make sure your question is part of acceptable categories"
                      hPutStrLn hdl (show (cats !! 0))
                    else do
                      -- DEBUG CODE: REMOVE BEFORE DEMO
                      --let probs = foldr (\h acc -> ((classify chat_classifier question),(probabilityForCategory chat_classifier question (classify chat_classifier question))):acc) [] cleanq
                      --hPutStrLn hdl (show probs)

                      -- get category, prob of valid answer
                      let category = classify chat_classifier question
                      let prob = probabilityForCategory chat_classifier question category
                      let ansDir = (curdDir ++ "/project/src/answers.csv")

                      -- if prob of answer is above a threshold return a response
                      -- otherwise return fail message for no valid response found with training data
                      if prob > -5.0
                        then do
                          -- fetch answer
                          str <- fetchAnswer ansDir (dropWhile (==' ') category)
                          hPutStrLn hdl ("Chatbot: " ++ str)
                        
                          -- check if answer was good to improve training data
                          hPutStrLn hdl "Chatbot: To help us improve, was this answer helpful? (y/n)"
                          answer <- fmap init (hGetLine hdl)
                        
                          if answer == "y"
                            then do
                              --- if answer is good, add question with category to questions.csv to improve future responses
                              write <- writeCsvR (curdDir ++ "/project/src/questions.csv") (dropWhile (==' ') category) question
                              hPutStrLn hdl "Chatbot: If you need anymore help feel free to ask another question, otherwise type 'quit' to exit"
                            else
                              -- nothing
                              hPutStrLn hdl "Chatbot: Sorry, please ask the question again for a more relevant answer"
                        else do
                          -- if training mode is enabled we can directly add new categories/questions/answers
                          if training == "y" 
                            then do
                              hPutStrLn hdl "Add to existing category (y/n)?"
                              existing <- fmap init (hGetLine hdl)
                              if existing == "y"
                                then do
                                  hPutStrLn hdl "Training Enabled please enter a Category"
                                  cat <- fmap init (hGetLine hdl)
                                  write <- writeCsvR (curdDir ++ "/project/src/questions.csv") cat question
                                  hPutStrLn hdl "Question added under category, you may ask another question"
                                else do
                                  hPutStrLn hdl "Training Enabled please enter a answer"
                                  ans <- fmap init (hGetLine hdl)
                                  hPutStrLn hdl "Training Enabled please enter a Category"
                                  cat <- fmap init (hGetLine hdl)
                                  hPutStrLn hdl ("Category: " ++ cat)
                                  write <- writeCsvR (curdDir ++ "/project/src/questions.csv") cat question
                                  write <- writeCsv (curdDir ++ "/project/src/answers.csv") cat ans
                                  hPutStrLn hdl "Question answer pair added, you may ask another question"
                            else 
                              hPutStrLn hdl "No relevant answer found, you may ask another question"
                  -- finally reloop to continue waiting for respones from the socket
                  loop
      hClose hdl                             -- close the handle