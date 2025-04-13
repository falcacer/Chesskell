{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE RankNTypes #-}

module Utils where

import           Common
import           Data.List (find)


-------------------------------------------------------------------------------

setupBoard :: [Piece]
setupBoard = whitePieces ++ whitePawns ++ blackPawns ++ blackPieces
  where
    whitePieces = [
        Piece $ Rook White (Pos 'a' 1),
        Piece $ Knight White (Pos 'b' 1),
        Piece $ Bishop White (Pos 'c' 1),
        Piece $ Queen White (Pos 'd' 1),
        Piece $ King White (Pos 'e' 1),
        Piece $ Bishop White (Pos 'f' 1),
        Piece $ Knight White (Pos 'g' 1),
        Piece $ Rook White (Pos 'h' 1)
      ]
    blackPieces = [
        Piece $ Rook Black (Pos 'a' 8),
        Piece $ Knight Black (Pos 'b' 8),
        Piece $ Bishop Black (Pos 'c' 8),
        Piece $ Queen Black (Pos 'd' 8),
        Piece $ King Black (Pos 'e' 8),
        Piece $ Bishop Black (Pos 'f' 8),
        Piece $ Knight Black (Pos 'g' 8),
        Piece $ Rook Black (Pos 'h' 8)
      ]
    whitePawns = [Piece $ Pawn White (Pos c 2) | c <- ['a'..'h']]
    blackPawns = [Piece $ Pawn Black (Pos c 7) | c <- ['a'..'h']]

initialGameState :: GameState
initialGameState = GameState {
    board = setupBoard,
    currentPlayer = White,
    moveHistory = [],
    capturedPieces = [],
    inCheck = False,
    gameResult = Nothing
}

pieceFromTypeWithPos :: String -> Color -> Position -> Piece
pieceFromTypeWithPos "King" color pos = Piece (King color pos)
pieceFromTypeWithPos "Queen" color pos = Piece (Queen color pos)
pieceFromTypeWithPos "Rook" color pos = Piece (Rook color pos)
pieceFromTypeWithPos "Bishop" color pos = Piece (Bishop color pos)
pieceFromTypeWithPos "Knight" color pos = Piece (Knight color pos)
pieceFromTypeWithPos "Pawn" color pos = Piece (Pawn color pos)
pieceFromTypeWithPos name _ _ = error $ "Invalid piece name: " ++ name

charToPiece :: Char -> Position -> Piece
charToPiece 'K' p = Piece (King Black p)
charToPiece 'Q' p = Piece (Queen Black p)
charToPiece 'R' p = Piece (Rook Black p)
charToPiece 'S' p = Piece (Bishop Black p)
charToPiece 'N' p = Piece (Knight Black p)
charToPiece 'P' p = Piece (Pawn Black p)
charToPiece 'k' p = Piece (King White p)
charToPiece 'q' p = Piece (Queen White p)
charToPiece 'r' p = Piece (Rook White p)
charToPiece 's' p = Piece (Bishop White p)
charToPiece 'n' p = Piece (Knight White p)
charToPiece 'p' p = Piece (Pawn White p)
charToPiece c   _ = error $ "Invalid piece: " ++ [c]


pieceNameToPromotionPiece :: Char -> PromotionPiece
pieceNameToPromotionPiece 'Q' = PromoteToQueen
pieceNameToPromotionPiece 'R' = PromoteToRook
pieceNameToPromotionPiece 'S' = PromoteToBishop
pieceNameToPromotionPiece 'N' = PromoteToKnight
pieceNameToPromotionPiece _ = PromoteToQueen

promotePieceName :: PromotionPiece -> Position -> Color -> Piece
promotePieceName PromoteToQueen pos color = Piece (Queen color pos)
promotePieceName PromoteToRook pos color = Piece (Rook color pos)
promotePieceName PromoteToBishop pos color = Piece (Bishop color pos)
promotePieceName PromoteToKnight pos color = Piece (Knight color pos)

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White


extractChessPiece :: Piece -> (forall p. ChessPiece p => p -> a) -> a
extractChessPiece (Piece p) f = f p

getPieceAtPosition :: Position -> [Piece] -> Maybe Piece
getPieceAtPosition targetPos pieces = 
    find (\piece -> extractChessPiece piece (\p -> piecePosition p) == targetPos) pieces

updatePiecePosition :: Position -> Piece -> Piece
updatePiecePosition newPos piece = extractChessPiece piece (\p -> pieceFromTypeWithPos (pieceName p) (pieceColor p) newPos)

findKing :: Color -> [Piece] -> Maybe Piece
findKing color pieces = find isKingOfColor pieces
  where
    isKingOfColor piece = extractChessPiece piece (\p -> 
        pieceName p == "King" && pieceColor p == color)
  
canCapture :: Piece -> Position -> GameState -> Bool
canCapture piece targetPos state =
    extractChessPiece piece (\p -> 
        captureRule p targetPos && isClearPath p targetPos (board state))

isKingInCheck :: Color -> GameState -> Bool
isKingInCheck color state = 
    case findKing color (board state) of
        Nothing -> False
        Just kingPiece ->
            let kingPosition = extractChessPiece kingPiece piecePosition
                opponentPieces = filter (\p -> extractChessPiece p pieceColor /= color) (board state)
            in any (\p -> canCapture p kingPosition state) opponentPieces 