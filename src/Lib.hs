module Lib
    ( someFunc
    ) where

type WordList = [String]


minWordLength :: Int 
minWordLength = 6


maxWordLength :: Int 
maxWordLength = 9


allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt" 
    return (lines dict)


gameLength :: String -> Bool
gameLength w =
    let l = length (w :: String)
    in l > minWordLength && l < maxWordLength


gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)


someFunc :: IO ()
someFunc =
    gameWords >>= print
