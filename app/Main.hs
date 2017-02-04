module Main where

import Data.Char (toLower)

import Game
import Word

main :: IO ()
main = do
    word <- getRandoWord
    let puzzle = freshPuzzle (fmap toLower word) 
    runGame puzzle
