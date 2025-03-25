module Common 
    ( Position(..)
    -- , Color(..)
    , PieceName(..)
    , PieceInfo(..)
    , Piece(..)
    , Column(..)
    , Row(..)
    , MoveType(..)
    , CastleType(..)
    , PromotionPiece(..)
    , Move(..)
    , Game(..)
    -- , charToColor
    , pieceFromChar
    ) where

    data Position = Pos Char Int deriving (Show, Eq)

    -- data Color = White | Black deriving (Show, Eq)

    data PieceName = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

    data PieceInfo = Info Int Position deriving (Show, Eq)

    data Piece = MkPiece PieceName PieceInfo deriving (Show, Eq)
    
    data CastleType = KingSide | QueenSide deriving (Show, Eq)
    
    data PromotionPiece = PromoteToQueen | PromoteToRook | PromoteToBishop | PromoteToKnight
        deriving (Show, Eq)
    
    data MoveType = Normal 
                  | Capture 
                  | Castle CastleType 
                  | EnPassant 
                  | Promotion PromotionPiece
                  deriving (Show, Eq)

    data Move = Move Piece MoveType Position deriving (Show, Eq)
    
    data Game = Game [Move] deriving (Show, Eq)

    -- charToColor :: Char -> Maybe Color
    -- charToColor 'W' = Just White
    -- charToColor 'L' = Just Black
    -- charToColor _ = Nothing

    pieceFromChar :: Char -> PieceName
    pieceFromChar 'K' = King
    pieceFromChar 'Q' = Queen
    pieceFromChar 'R' = Rook
    pieceFromChar 'B' = Bishop
    pieceFromChar 'N' = Knight
    pieceFromChar 'P' = Pawn
    pieceFromChar c   = error $ "Invalid piece: " ++ [c]

