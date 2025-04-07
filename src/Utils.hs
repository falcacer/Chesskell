{-# LANGUAGE RankNTypes #-}
module Utils where

import           Common


-------------------------------------------------------------------------------

setupBoard :: [Piece]
setupBoard = 
    [pieceFromTypeWithPos pieceType White (Pos c 1) | 
        (pieceType, c) <- zip ["Rook", "Knight", "Bishop", "Queen", "King", "Bishop", "Knight", "Rook"] ['a'..'h']] ++
    
    [Piece (Pawn White (Pos c 2)) | c <- ['a'..'h']] ++
    
    [Piece (Pawn Black (Pos c 7)) | c <- ['a'..'h']] ++
    
    [pieceFromTypeWithPos pieceType Black (Pos c 8) | 
        (pieceType, c) <- zip ["Rook", "Knight", "Bishop", "Queen", "King", "Bishop", "Knight", "Rook"] ['a'..'h']]

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
charToPiece 'B' p = Piece (Bishop Black p)
charToPiece 'N' p = Piece (Knight Black p)
charToPiece 'P' p = Piece (Pawn Black p)
charToPiece 'k' p = Piece (King White p)
charToPiece 'q' p = Piece (Queen White p)
charToPiece 'r' p = Piece (Rook White p)
charToPiece 'b' p = Piece (Bishop White p)
charToPiece 'n' p = Piece (Knight White p)
charToPiece 'p' p = Piece (Pawn White p)
charToPiece c   _ = error $ "Invalid piece: " ++ [c]

extractChessPiece :: Piece -> (forall p. ChessPiece p => p -> a) -> a
extractChessPiece (Piece p) f = f p
