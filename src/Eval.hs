{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Eval where

import Common
import Monads
import Utils
import Data.List (delete)


-------------------------------------------------------------------------------

moveM :: Piece -> Position -> ChessState Bool
moveM piece goal =
    if extractChessPiece piece (\p -> moveRule p goal)
        then return True
        else invalidMoveError piece goal "Invalid movement pattern"

captureM :: Piece -> Position -> ChessState Bool
captureM piece goal =
    if extractChessPiece piece (\p -> captureRule p goal)
        then return True
        else invalidMoveError piece goal "Invalid capture pattern"

isClearPathM :: Piece -> Position -> ChessState Bool
isClearPathM piece goal = do 
    state <- getState
    if extractChessPiece piece (\p -> isClearPath p goal (board state))
        then return True
        else blockedPathError piece goal []

colorMatch :: Piece -> ChessState Bool
colorMatch piece = do
    state <- getState
    let isCorrectTurn = extractChessPiece piece (\p -> pieceColor p == currentPlayer state)
    if isCorrectTurn
        then return True
        else wrongTurnError piece (currentPlayer state)

promotion :: Move -> PromotionPiece -> ChessState Bool
promotion move@(Move piece _ _) new_piece =
    let piece_pos = extractChessPiece piece piecePosition
        piece_col = extractChessPiece piece pieceColor
        promoted_piece = promotePieceName new_piece piece_pos piece_col
    in do
        state <- getState
        modifyState $ \s -> s
            { board = promoted_piece : delete piece (board state)
            , moveHistory = move : moveHistory state
            , currentPlayer = if currentPlayer state == White then Black else White
            }
        return True

normal :: Move -> ChessState Bool
normal move@(Move piece _ target) = do
    _ <- moveM piece target
    _ <- isClearPathM piece target   
    state <- getState
    
    case getPieceAtPosition target (board state) of
        Just _ -> throwError $ SelfCapture piece target
        Nothing -> do
            let piece_color = extractChessPiece piece pieceColor
                new_pos_piece = updatePiecePosition target piece
                new_board = new_pos_piece : delete piece (board state)
                new_state = state { board = new_board }

            if isKingInCheck piece_color new_state
                then throwError $ MoveIntoCheck piece target
                else do
                    let next_player = oppositeColor piece_color
                    modifyState (\s -> s {
                        board = new_board,
                        moveHistory = move : moveHistory state,
                        currentPlayer = next_player,
                        inCheck = isKingInCheck next_player state
                    })
                    return True
   
capture :: Move -> ChessState Bool
capture move@(Move piece _ target) = do
    state <- getState
    _ <- captureM piece target
    _ <- isClearPathM piece target
    
    case getPieceAtPosition target (board state) of 
        Nothing -> throwError $ PieceNotFound target
        Just target_piece -> do 
            let target_color = extractChessPiece target_piece pieceColor  
                piece_color = extractChessPiece piece pieceColor           
            if target_color == piece_color
                then throwError $ SelfCapture piece target
                else do
                    let new_pos = updatePiecePosition target piece
                        new_board = new_pos : delete piece (delete target_piece (board state))
                        new_state = state { board = new_board }

                    if isKingInCheck piece_color new_state
                        then throwError $ MoveIntoCheck piece target
                        else do
                            let next_player = oppositeColor piece_color
                            modifyState $ \s -> s
                                { board = new_board
                                , moveHistory = move : moveHistory state
                                , capturedPieces = target_piece : capturedPieces state
                                , currentPlayer = next_player
                                , inCheck = isKingInCheck next_player state
                                }
                            return True


makeMove :: Move -> ChessState Bool
makeMove move@(Move piece moveType _) = do
    state <- getState

    case gameResult state of
        Just result -> throwError $ GameOver result
        Nothing -> return ()

    if not (elem piece (board state))
        then throwError $ PieceNotFound (extractChessPiece piece piecePosition)
        else do
            _ <- colorMatch piece

            case moveType of
                Normal -> do normal move
                Capture -> capture move
                Promotion new_piece -> promotion move new_piece

evalMoves :: Moves -> ChessState Bool
evalMoves [] = return True 
evalMoves (move:moves) = do
    success <- makeMove move
    if success
        then evalMoves moves
        else return False
