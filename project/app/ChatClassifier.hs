  
module ChatClassifier where

import System.IO
import BayesClassifier
import Data.List (intersect)

-- seperate CSV on commas and create a list of elements
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep s = foldr (\e (h:t) -> if s e then []:(h:t) else (e:h):t) [[]] 

-- Read the CSV, and split lines on commas
readCsv :: FilePath -> IO [[String]]
readCsv filename = do
    file <- readFile filename
    return [splitsep (==',') row | row <- ( splitsep (=='\n') file )]

-- Write to the CSV for category answer pairs
writeCsv :: FilePath -> [Char] -> [Char] -> IO ()
writeCsv filename c q = appendFile filename ("\n" ++ c ++ "," ++ q)

-- Write to the CSV for a new category
writeCat :: FilePath -> [Char] -> IO ()
writeCat filename c = appendFile filename ("," ++ c)

-- Write to the CSV for question category pairs
writeCsvR :: FilePath -> [Char] -> [Char] -> IO ()
writeCsvR filename c q = appendFile filename ("\n" ++ q ++ "," ++ c)

-- Fetch the answer with matching category from CSV
fetchAnswer :: FilePath -> Category -> IO String
fetchAnswer filename c = do
    file <- readFile filename
    let list =  [splitsep (==',') row | row <- ( splitsep (=='\n') file )]
    let search = [r | (h:r) <- list, h == c]
    let resp = search !! 0 !! 0
    return resp

-- Train BayesClassifier for a single line
trainline:: Classifier -> [String] -> Classifier
trainline cls a = train cls (a !! 0) (a !! 1)

-- Train Bayes Classifier given a training CSV
trainedClassifier :: FilePath -> IO Classifier
trainedClassifier fp = do
    trainingData <- readCsv fp
    let trained_classifier = foldl trainline classifier trainingData
    return trained_classifier

-- from https://stackoverflow.com/questions/27471710/checking-if-2-list-have-any-equal-element-haskell
compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = not . null . intersect a
