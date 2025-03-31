module Main where

import Common
import Eval
import PrettyPrint
import Monads

main :: IO ()
main = do
    let initialState = initialGameState
    putStrLn "Initial Board:"
    putStrLn $ displayGame initialState

    -- Example moves
    let moves = [Move (MkPiece King Black (Info 0 (Pos 'e' 8))) Normal (Pos 'e' 6),
                 Move (MkPiece King White (Info 0 (Pos 'e' 1))) Normal (Pos 'e' 3)]

    -- Apply moves
    let game = do
            mapM_ evalMove moves

    case runGame game of
        Nothing -> putStrLn "Invalid move sequence!"
        Just (_, finalState) -> do
            putStrLn "Final Board:"
            putStrLn $ displayGame finalState