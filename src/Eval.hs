module Eval where

import Common
import Monads
import Utils
import Data.List (delete)


-------------------------------------------------------------------------------

updatePiecePosition :: Position -> Piece -> Piece
updatePiecePosition newPos piece = extractChessPiece piece (\p -> pieceFromTypeWithPos (pieceName p) (pieceColor p) newPos)

moveM :: Piece -> Position -> ChessState Bool
moveM piece goal = return $ extractChessPiece piece (\p -> move p goal)

colorMatch :: Piece -> ChessState Bool
colorMatch piece = do
    state <- getState
    return $ extractChessPiece piece (\p -> pieceColor p == currentPlayer state)

captureM :: Piece -> Position -> ChessState Bool
captureM piece goal = return $ extractChessPiece piece (\p -> capture p goal)

pathIsClearM :: Piece -> Position -> ChessState Bool
pathIsClearM piece goal = return $ extractChessPiece piece (\p -> pathIsClear p goal)

promotePiece :: String -> Color -> Position -> Maybe Piece
promotePiece "Queen" color pos = Just (Piece (Queen color pos))
promotePiece "Rook" color pos = Just (Piece (Rook color pos))
promotePiece "Bishop" color pos = Just (Piece (Bishop color pos))
promotePiece "Knight" color pos = Just (Piece (Knight color pos))
promotePiece _ _ _ = Nothing


promotePieceName :: PromotionPiece -> String
promotePieceName PromoteToQueen = "Queen"
promotePieceName PromoteToRook = "Rook"
promotePieceName PromoteToBishop = "Bishop"
promotePieceName PromoteToKnight = "Knight"


checkPieceOnCapture :: Move -> String -> ChessState Bool
checkPieceOnCapture move@(Move piece _ _) new_piece_name = do
    state <- getState
    let captured_pieces = capturedPieces state
        piece_col = extractChessPiece piece (\p -> pieceColor p)
        col_captured_piece = filter (\p -> piece_col == extractChessPiece p (\p -> pieceColor p)) captured_pieces
        target_piece = elem new_piece_name (map (\p -> (extractChessPiece p (\p -> pieceName p))) col_captured_piece)    
    if target_piece 
        then do 
            let piece_pos = extractChessPiece piece (\p -> piecePosition p)
                new_piece = promotePiece new_piece_name piece_col piece_pos
            case new_piece of
                Just n_piece -> do
                    modifyState (\s -> s {
                        board = n_piece : delete piece (board state), 
                        moveHistory = move : moveHistory state,
                        capturedPieces = filter (\p -> piece_col /= extractChessPiece p (\p -> pieceColor p) && 
                                              new_piece_name /= extractChessPiece p (\p -> pieceName p)) captured_pieces,
                        currentPlayer = if currentPlayer state == White then Black else White
                    })
                    return True
                Nothing -> return False
        else return False

promotion :: Move -> PromotionPiece -> ChessState Bool
promotion move@(Move piece _ _) new_piece = do
    state <- getState
    let new_piece_name = promotePieceName new_piece
    checkPieceOnCapture move new_piece_name


normal :: Move -> ChessState Bool
normal move@(Move piece _ goal) = do
    state <- getState
    is_valid <- moveM piece goal
    is_path_clear <- pathIsClearM piece goal
    let piece_in_goal = getPieceAtPosition goal (board state)
    case piece_in_goal of 
        Nothing -> if is_valid && is_path_clear then let new_pos = updatePiecePosition goal piece
                                                            in do modifyState (\s -> s { 
                                                                                        board = new_pos : delete piece (board state), 
                                                                                        moveHistory = move : moveHistory state,
                                                                                        currentPlayer = if currentPlayer state == White then Black else White
                                                                                       })
                                                                  return True
                                                       else do return False
        Just _ -> return False
   
takePiece :: Move -> ChessState Bool
takePiece move@(Move piece _ goal) = do
    state <- getState
    is_valid <- captureM piece goal
    is_path_clear <- pathIsClearM piece goal
    let piece_in_goal = getPieceAtPosition goal (board state)
    case piece_in_goal of 
        Nothing -> return False
        Just target_piece -> do 
            color_match <- colorMatch target_piece
            if is_valid && is_path_clear && not color_match 
            then 
                let new_pos = updatePiecePosition goal piece
                in do 
                    modifyState (\s -> s { 
                        board = new_pos : delete piece (delete target_piece (board state)),
                        moveHistory = move : moveHistory state, 
                        capturedPieces = target_piece : capturedPieces state,
                        currentPlayer = if currentPlayer state == White then Black else White
                    })
                    return True
            else return False

makeMove :: Move -> ChessState Bool
makeMove move@(Move piece moveType _) = do
    state <- getState
    color_match <- colorMatch piece
    if elem piece (board state) && color_match then 
        case moveType of
            Normal -> normal move
            Capture -> takePiece move
            Promotion new_piece -> promotion move new_piece
    else do return False

evalMoves :: Moves -> ChessState Bool
evalMoves [] = return True 
evalMoves (move:moves) = do
    success <- makeMove move
    if success
        then evalMoves moves
        else return False
