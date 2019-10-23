    -- CODE REFERENCE: http://vishnumenon.com/2012/06/07/naive-bayes-classifier-in-haskell/
    -- Source for the code listed below
    
    module BayesClassifier where
    -- Text Classifier Using Bayes Formula
    import Data.List
    import Data.Char
    -- Category of type String
    type Category = String
    -- Classifier a new type that consists of a category and a list of strings, it
    -- derives eq, and show for comparison and visulization of the classified data
    newtype Classifier = Classifier { training :: [(Category, [String])] } deriving (Eq, Show)

    -- Get a new classifer with no training
    classifier :: Classifier
    classifier = Classifier []

    -- classifier probabilities
    probabilityOfWordInCategory :: Classifier -> String -> Category -> Double
    -- Adding + 1 for Laplacian Correction to correct worse case comparisons
    probabilityOfWordInCategory (Classifier training) word category = let allInCategory = filter (\(cat, _) -> cat == category) training
                                                                          allInCategoryContainingWord = filter (\(_, text) -> word `elem` text) allInCategory
                                                                      in (fromIntegral $ length allInCategoryContainingWord + 1) / (fromIntegral $ length allInCategory + 1)

    -- determines the probability of a category using the training data
    probabilityOfCategory :: Classifier -> Category -> Double
    probabilityOfCategory (Classifier training) category =  let allInCategory = filter (\(cat, _) -> cat == category) training 
                                                            in (fromIntegral $ length allInCategory) / (fromIntegral $ length training)

    -- Train a classifier using a give string
    train :: Classifier -> String -> Category -> Classifier 
    train (Classifier training ) text category = Classifier $ (category, cleanInput $ text):training

    -- Categorize text with a classifier
    classify :: Classifier -> String -> Category
    classify classifier text = fst $ head $ sortBy (\(_, a) (_, b)  -> b `compare` a) $ probabilities classifier text

    -- Get Probability for each Category
    probabilities :: Classifier ->  String -> [(Category, Double)] 
    probabilities classifier@(Classifier training) text =  map (\cat -> (cat, probabilityForCategory classifier text cat)) $ nub $ map (\(cat, _) -> cat) training

    -- Get Probability for a passage in a certain category
    probabilityForCategory :: Classifier -> String -> Category -> Double
    probabilityForCategory classifier text category = (+) (log $ probabilityOfCategory classifier category)  (sum $ map (\word -> log $ probabilityOfWordInCategory classifier word category) $ cleanInput text) 

    -- Lowercase, Remove Punctuation
    cleanInput :: String -> [String]
    cleanInput text = filter (\w -> not (w `elem` stopWords)) $ words $ filter (`elem` ' ':['a'..'z']) $ map toLower text 
                      where stopWords = [",","'","a","about","above","after","again","against","all","am","an","and","any","are","aren't","as","at","be","because","been","before","being","below","between","both","but","by","can't","cant","cannot","could","couldn't","did","didn't","do","does","doesn't","doing","don't","down","during","each","few","for","from","further","had","hadn't","has","hasn't","have","haven't","having","he","he'd","he'll","he's","her","here","here's","hers","herself","him","himself","his","how","how's","i","i'd","i'll","i'm","i've","if","in","into","is","isn't","it","it's","its","itself","let's","me","more","most","mustn't","my","myself","no","nor","not","of","off","on","once","only","or","other","ought","our","ours ","ourselves","out","over","own","same","shan't","she","she'd","she'll","she's","should","shouldn't","so","some","such","than","that","that's","the","their","theirs","them","themselves","then","there","there's","these","they","they'd","they'll","they're","they've","this","those","through","to","too","under","until","up","very","was","wasn't","we","we'd","we'll","we're","we've","were","weren't","what","what's","when","when's","where","where's","which","while","who","who's","whom","why","why's","with","won't","would","wouldn't","you","you'd","you'll","you're","you've","your","yours","yourself","yourselves"]
   