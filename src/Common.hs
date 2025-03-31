module Common where

import qualified Data.Map as Map
import Data.Map (Map)

data Position = Pos Char Int deriving (Show, Eq, Ord)

data PieceName = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

data PieceInfo = Info Int Position deriving (Show, Eq)

data Color = White | Black deriving (Show, Eq)

data Piece = MkPiece PieceName Color PieceInfo deriving (Show, Eq)

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

type Moves = [Move]

data GameResult = WhiteWin | BlackWin | Draw deriving (Show, Eq)

-- Define a data type to represent the state of the chess game
data GameState = GameState {
    board :: Map Position Piece,      -- Current board position
    currentPlayer :: Color,           -- Whose turn it is
    moveHistory :: [Move],            -- History of moves
    capturedPieces :: [Piece],        -- Pieces that have been captured
    inCheck :: Bool,                  -- Whether the current player is in check
    gameResult :: Maybe GameResult    -- Result of the game (if it's over)
} deriving (Show, Eq)

-- Initialize a new game state with pieces in their starting positions
initialGameState :: GameState
initialGameState = GameState {
    board = setupBoard,
    currentPlayer = White,
    moveHistory = [],
    capturedPieces = [],
    inCheck = False,
    gameResult = Nothing
}

-- Setup the initial chess board
setupBoard :: Map Position Piece
setupBoard = Map.fromList $
    -- White pieces
    [((Pos c 1), MkPiece pieceType White (Info 0 (Pos c 1))) | 
        (c, pieceType) <- zip "abcdefgh" [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]] ++
    -- White pawns
    [((Pos c 2), MkPiece Pawn White (Info 0 (Pos c 2))) | c <- ['a'..'h']] ++
    -- Black pawns
    [((Pos c 7), MkPiece Pawn Black (Info 0 (Pos c 7))) | c <- ['a'..'h']] ++
    -- Black pieces
    [((Pos c 8), MkPiece pieceType Black (Info 0 (Pos c 8))) | 
        (c, pieceType) <- zip "abcdefgh" [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]]

-- Convert a character to a piece name and color
pieceFromChar :: Char -> (PieceName, Color)
pieceFromChar 'K' = (King, Black)
pieceFromChar 'Q' = (Queen, Black)
pieceFromChar 'R' = (Rook, Black)
pieceFromChar 'B' = (Bishop, Black)
pieceFromChar 'N' = (Knight, Black)
pieceFromChar 'P' = (Pawn, Black)
pieceFromChar 'k' = (King, White)
pieceFromChar 'q' = (Queen, White)
pieceFromChar 'r' = (Rook, White)
pieceFromChar 'b' = (Bishop, White)
pieceFromChar 'n' = (Knight, White)
pieceFromChar 'p' = (Pawn, White)
pieceFromChar c   = error $ "Invalid piece: " ++ [c]

