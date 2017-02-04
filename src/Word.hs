module Word
    ( getRandoWord
    ) where

import System.Random (randomRIO)

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
    in  l > minWordLength && l < maxWordLength

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)

randomWords :: WordList -> IO String
randomWords wl = do
    randomIndex <- randomRIO(0, maxIndex)
    return $ wl !! randomIndex
    where 
        maxIndex = length wl - 1

getRandoWord :: IO String
getRandoWord =
    gameWords >>= randomWords
