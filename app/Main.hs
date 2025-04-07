module Main where

import           Parse 
import           Utils                       
import           Common
import           Eval
import           Monads
import           PrettyPrint
import           System.Environment (getArgs)
import           System.IO (readFile)

-------------------------------------------------------------------------------

-- Helper function to display test results
assertTest :: String -> Bool -> IO ()
assertTest desc success = putStrLn $ if success then "✓ " ++ desc else "✗ " ++ desc

-- Process moves from a string input
processMoveString :: String -> IO ()
processMoveString input = do
    putStrLn "Processing moves from input"
    putStrLn $ "Input: " ++ input
    
    -- Parse the input string
    case lexer input of
        tokens -> case parse tokens of
            Ok moves -> do
                putStrLn $ "Parsed " ++ show (length moves) ++ " moves: " ++ show moves
                
                -- Evaluate the moves
                let result = runChess (evalMoves moves) initialGameState
                
                case result of
                    Just (success, finalState) -> do
                        putStrLn "Initial board:"
                        putStrLn $ printBoard initialGameState
                        
                        assertTest "All moves succeeded" success
                        
                        -- Display additional game information
                        putStrLn $ displayGame finalState
                        
                    Nothing -> putStrLn "Move sequence processing failed unexpectedly"
                    
            Failed err -> putStrLn $ "Parse error: " ++ err


-- Run the program with file input or default test
main :: IO ()
main = do
    putStrLn "Cheskell - Haskell Chess Engine"
    putStrLn "=============================="
    
    args <- getArgs
    case args of
        [filename] -> do
            putStrLn $ "Reading moves from file: " ++ filename
            content <- readFile filename
            processMoveString content
            
        [] -> do
            putStrLn "No input file provided. Using default test."
            
        _ -> putStrLn "Usage: cheskell [move_file.txt]"
    
    putStrLn "\nProgram completed."