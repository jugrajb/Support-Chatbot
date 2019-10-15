  
module ChatClassifier where

import System.IO
import BayesClassifier

splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep _ [] = []
splitsep s l = (fst fs):(splitsep s (snd fs))
    where fs = firstsep s [] l

firstsep:: (a -> Bool) -> [a] -> [a] -> ([a], [a])
firstsep _ p [] = (p,[])
firstsep s p (h:t) 
    | s h = (p, t)
    | otherwise = firstsep s (p++[h]) t


splitcsv :: FilePath -> IO [[String]]
splitcsv filename =
    do
    filecontent <- readFile filename
    let lines = splitsep (=='\r') filecontent
    let lines_split = [splitsep (==',') line | line <- lines]
    return lines_split


trainline:: Classifier -> [String] -> Classifier
trainline cls a = train cls (a !! 0) (a !! 1)


trainedClassifier :: FilePath -> IO Classifier
trainedClassifier fp = do
    trainingData <- splitcsv fp
    let trained_classifier = foldl trainline classifier trainingData
    return trained_classifier

{--
interactionLoop myClassifier function = case function of 
                                            "start" ->  
                                                do
                                                    putStrLn "Enter an action [train|classify]"
                                                    action <- getLine
                                                    interactionLoop myClassifier action
                                            "train" -> 
                                                do
                                                    putStr "Category: "
                                                    category <- getLine
                                                    putStr "Material: "
                                                    material <- getLine
                                                    interactionLoop (train myClassifier material category) "start"
                                            "classify" ->
                                                do
                                                    putStr "Material: "
                                                    material <- getLine
                                                    putStrLn $ classify myClassifier material
                                                    --putStrLn . show $ probabilities myClassifier material
                                                    --putStrLn "\n\n\n\n"
                                                    interactionLoop myClassifier "start"
                                            _ ->
                                                    interactionLoop myClassifier "start"
main = do
        trainingData <- splitcsv "htraining.csv"
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin NoBuffering
        trained_classifier = foldl trainline classifier trainingData


--}
        
