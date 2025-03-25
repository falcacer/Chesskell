module Main where

import Parse (parser)
import Common

-- Test parsing various chess notations
main :: IO ()
main = do
    putStrLn "Testing Chess EDSL Parser"
    putStrLn "========================="
    
    -- Test single moves
    testParser "K-e1" "King move to e1"
    testParser "Q-d8" "Queen move to d8"
    testParser "Rxh8" "Rook captures on h8"
    
    -- Test multiple moves
    testParser "K-e1;Q-d8" "King to e1 then Queen to d8"
    testParser "Rxh8;P-a4" "Rook captures on h8 then Pawn to a4"
    testParser "N-c3;Bxf6;Q-h4" "Knight to c3, Bishop captures on f6, Queen to h4"
    
    -- Note: Invalid inputs will crash the program without exception handling
    -- To test invalid inputs, uncomment these lines one at a time
    -- testParser "Z-e1" "Invalid piece"
    -- testParser "K-j9" "Invalid position"
    -- testParser "K-e1;" "Incomplete move sequence"

-- Helper function to test the parser with a given input
testParser :: String -> String -> IO ()
testParser input description = do
    putStrLn $ "\nTesting: " ++ description
    putStrLn $ "Input: " ++ input
    putStrLn "Result:"
    let result = parser input
    putStrLn $ "  " ++ show result