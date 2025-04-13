{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import           Parse 
import           Utils
import           Common
import           Eval
import           Monads
import           PrettyPrint
import           System.Environment (getArgs)

-------------------------------------------------------------------------------

processMovesOneByOne :: [Move] -> GameState -> IO GameState
processMovesOneByOne [] currentState = return currentState
processMovesOneByOne (move:moves) currentState = do
    case runGame (makeMove move) currentState of
        Right (_, newState) -> processMovesOneByOne moves newState
        Left err -> do
            putStrLn $ "Error en el movimiento: " ++ show move
            putStrLn $ "Detalle del error: " ++ show err
            return currentState

processMoveString :: String -> IO ()
processMoveString input = do
    putStrLn $ "Input: " ++ input
    
    case lexer input of
        tokens -> case parse tokens of
            Ok moves -> do
                putStrLn $ "Parsed " ++ show (length moves) ++ " moves: " ++ show moves
                
                putStrLn "Initial board:"
                putStrLn $ printBoard initialGameState
                
                finalState <- processMovesOneByOne moves initialGameState
                
                putStrLn "Final state (last valid position):"
                putStrLn $ displayGame finalState
                    
            Failed err -> putStrLn $ "Parse error: " ++ err

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