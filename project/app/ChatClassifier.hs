  
module ChatClassifier where

import System.IO
import BayesClassifier

splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep s = foldr (\e (h:t) -> if s e then []:(h:t) else (e:h):t) [[]] 

readCsv :: FilePath -> IO [[String]]
readCsv filename = do
    file <- readFile filename
    return [splitsep (==',') row | row <- ( splitsep (=='\n') file )]

writeCsv :: FilePath -> [Char] -> [Char] -> IO ()
writeCsv filename c q = appendFile filename ("\n" ++ c ++ "," ++ q)

fetchAnswer :: FilePath -> Category -> IO String
fetchAnswer filename c = do
    file <- readFile filename
    let list =  [splitsep (==',') row | row <- ( splitsep (=='\n') file )]
    let search = [r | (h:r) <- list, h == c]
    let resp = search !! 0 !! 0
    return resp

trainline:: Classifier -> [String] -> Classifier
trainline cls a = train cls (a !! 0) (a !! 1)

trainedClassifier :: FilePath -> IO Classifier
trainedClassifier fp = do
    trainingData <- readCsv fp
    let trained_classifier = foldl trainline classifier trainingData
    return trained_classifier

        
