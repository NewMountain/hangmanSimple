module Lib
    ( getRandoWord
    , freshPuzzle
    , runGame
    ) where

import System.Random (randomRIO)
import Control.Monad (forever, when)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ hits guessed) =
        intersperse ' ' (fmap renderPuzzleChar hits) ++ " Guessed so far: " ++ guessed

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


freshPuzzle :: String -> Puzzle 
freshPuzzle str = 
    Puzzle str hits []
    where
        len = length str
        hits = replicate len Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle secret hits guessed) guess =
    guess `elem` secret


alreadyGuessed :: Puzzle -> Char -> Bool 
alreadyGuessed (Puzzle secret hits guessed) guess =
    guess `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = 
    fromMaybe '_'


fillInCharacter :: Puzzle -> Char -> Puzzle 
fillInCharacter (Puzzle secret hits guessed) guess =
    Puzzle secret newHits (guessed ++ [guess])
    where
        zipper g sChar gChar =
            if sChar == g
            then Just g
            else gChar
        newHits =
            zipWith (zipper guess) secret hits

handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess] 
    case (charInWord puzzle guess
         , alreadyGuessed puzzle guess) of 
        (_, True) -> do
            putStrLn "You already guessed that\
                     \ character, pick something else!"
            return puzzle 
        (True, _) -> do
            putStrLn "This character was in the word,\
                     \ filling in the word accordingly"
            return (fillInCharacter puzzle guess) 
        (False, _) -> do
            putStrLn "This character wasn't in\
                     \ the word, try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle secret hits guessed) =
    when (badGuesses > 8) $
        do
            putStrLn "You lose!"
            putStrLn $ "The word was: " ++ secret
            exitSuccess 
    where 
        successfulHits = length $ filter isJust hits
        badGuesses = length guessed - successfulHits

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ hits _) =
    when (all isJust hits) $
        do
            putStrLn "You win!"
            exitSuccess 

runGame :: Puzzle -> IO () 
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "\nCurrent puzzle is: " ++ show puzzle 
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame 
        _ ->putStrLn "Your guess must\
                     \ be a single character"