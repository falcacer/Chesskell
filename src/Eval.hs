module Eval where

import Common
import Monads
import Data.Map (Map)
import qualified Data.Map as Map

-- Evaluate a move: check if it's legal and update the game state
evalMove :: Move -> ChessState Bool
evalMove move = makeMove move


-- Function to make a move and update the game state
makeMove :: Move -> ChessState Bool
makeMove move@(Move piece moveType target) = do
    state <- getState

    -- Check if the move is valid
    let isValidMove = validateMove move state
    
    if not isValidMove
        then return False
        else do
            -- Update the game state
            modifyState (updateGameState move)
            return True

-- Validate if a move is legal given the current game state
validateMove :: Move -> GameState -> Bool
validateMove (Move (MkPiece pieceName color (Info _ start)) moveType end) state =
    -- Basic validation - this would need to be expanded for a real chess implementation
    case Map.lookup start (board state) of
        Nothing -> False  -- No piece at the start position
        Just (MkPiece actualPiece col _) -> 
            let basicMoveValid = isBasicMoveValid pieceName start end
                pathClear = isPathClear start end (board state)
                captureValid = validateCapture moveType end (board state)
            in basicMoveValid && pathClear && captureValid

-- Check if the basic move pattern is valid for the piece
isBasicMoveValid :: PieceName -> Position -> Position -> Bool
isBasicMoveValid pieceName (Pos c1 r1) (Pos c2 r2) =
    let dx = abs (fromEnum c2 - fromEnum c1)
        dy = abs (r2 - r1)
    in case pieceName of
        King -> dx <= 1 && dy <= 1
        Queen -> dx == 0 || dy == 0 || dx == dy
        Rook -> dx == 0 || dy == 0
        Bishop -> dx == dy
        Knight -> (dx == 1 && dy == 2) || (dx == 2 && dy == 1)
        Pawn -> dx == 0 && ((dy == 1) || (dy == 2 && r1 == 2))

-- Check if the path between two positions is clear (no pieces in the way)
-- This is a simplified version - would need more logic for specific pieces
isPathClear :: Position -> Position -> Map Position Piece -> Bool
isPathClear start end board = 
    -- This is a placeholder - would need actual implementation
    True

-- Check if a capture move is valid
validateCapture :: MoveType -> Position -> Map Position Piece -> Bool
validateCapture Normal _ _ = True  -- Normal moves don't need capture validation
validateCapture Capture pos board = 
    case Map.lookup pos board of
        Just _ -> True  -- Something to capture
        Nothing -> False  -- Nothing to capture

-- Update the game state after making a move
updateGameState :: Move -> GameState -> GameState
updateGameState (Move (MkPiece pieceName color (Info n start)) moveType end) state =
    let -- Get the piece being moved
        movedPiece = MkPiece pieceName color (Info (n+1) end)
        
        -- Check if there's a capture
        capturedPiece = Map.lookup end (board state)
        
        -- Update captured pieces list if needed
        newCapturedPieces = case capturedPiece of
            Just piece -> piece : capturedPieces state
            Nothing -> capturedPieces state
            
        -- Update the board with the new piece position
        newBoard = case moveType of
            Normal -> Map.insert end movedPiece (Map.delete start (board state))
            Capture -> Map.insert end movedPiece (Map.delete start (board state))
            _ -> board state  -- Simplified - would need handling for special moves
            
        -- Toggle the current player
        newPlayer = case currentPlayer state of
            White -> Black
            Black -> White
            
        -- Add the move to history
        newMoveHistory = (Move movedPiece moveType end) : moveHistory state
            
    in GameState {
        board = newBoard,
        currentPlayer = newPlayer,
        moveHistory = newMoveHistory,
        capturedPieces = newCapturedPieces,
        inCheck = False,  -- Simplified - would need to check this
        gameResult = Nothing  -- Simplified - would need to check this
    }

-- Example of executing a series of moves
playMoves :: [Move] -> ChessState (Maybe GameState)
playMoves moves = do
    results <- mapM makeMove moves
    if all id results
        then do
            finalState <- getState
            return $ Just finalState
        else
            return Nothing
