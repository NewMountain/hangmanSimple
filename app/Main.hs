module Main where

import Data.Char (toLower)

import Lib

main :: IO ()
main = do
    word <- getRandoWord
    let puzzle = freshPuzzle (fmap toLower word) 
    runGame puzzle
