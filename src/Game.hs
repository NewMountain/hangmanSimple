module Game
    ( freshPuzzle
    , runGame
    ) where


import Control.Monad (forever, when)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)



data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ hits guessed) =
        intersperse ' ' (fmap renderPuzzleChar hits) ++ " Guessed so far: " ++ guessed


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
