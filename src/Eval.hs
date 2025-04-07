module Eval where

import Common
import Monads
import Utils
import Data.List (delete)

-------------------------------------------------------------------------------

updatePiecePosition :: Position -> Piece -> Piece
updatePiecePosition newPos piece = extractChessPiece piece (\p -> pieceFromTypeWithPos (pieceName p) (pieceColor p) newPos)

isValidMoveM :: Piece -> Position -> ChessState Bool
isValidMoveM piece goal = do state <- getState
                             return $ extractChessPiece piece (\p -> isValidMove p goal)

pathIsClearM :: Piece -> Position -> ChessState Bool
pathIsClearM piece goal = do state <- getState
                             return $ extractChessPiece piece (\p -> pathIsClear p goal)

normal :: Move -> ChessState Bool
normal move@(Move piece moveType goal) = do
    state <- getState
    is_valid <- isValidMoveM piece goal
    is_path_clear <- pathIsClearM piece goal
    if moveType == Normal && is_valid && is_path_clear then let new_pos = updatePiecePosition goal piece
                                                            in do modifyState (\s -> s { 
                                                                                        board = new_pos : delete piece (board state), 
                                                                                        moveHistory = move : moveHistory state 
                                                                                       })
                                                                  return True
                                                       else do return False

makeMove :: Move -> ChessState Bool
makeMove move@(Move piece moveType goal) = do
    state <- getState
    if elem piece (board state) then normal move
                                else return False

evalMoves :: Moves -> ChessState Bool
evalMoves [] = return True 
evalMoves (move:moves) = do
    success <- makeMove move
    if success
        then evalMoves moves
        else return False
