module PrettyPrint where

import Common
import Data.Map (Map)
import qualified Data.Map as Map

-- Unicode characters for chess pieces
pieceToUnicode :: Piece -> String
pieceToUnicode (MkPiece pieceName color _) =
    let symbol = case pieceName of
            King   -> if color == White then "♔" else "♚"
            Queen  -> if color == White then "♕" else "♛"
            Rook   -> if color == White then "♖" else "♜"
            Bishop -> if color == White then "♗" else "♝"
            Knight -> if color == White then "♘" else "♞"
            Pawn   -> if color == White then "♙" else "♟"
    in symbol

-- Pretty-print the current board state
printBoard :: GameState -> String
printBoard state = unlines (
    -- Column markers (a-h)
    "    a b c d e f g h    " :
    -- Top border
    "  +-----------------+  " :
    -- Board rows (8 to 1) with pieces
    [printRow r (board state) | r <- [8,7..1]] ++
    -- Bottom border
    ["  +-----------------+  ",
     -- Column markers (repeated at bottom)
     "    a b c d e f g h    "])

-- Print a single row of the board
printRow :: Int -> Map Position Piece -> String
printRow r board =
    -- Row number on the left
    show r ++ " | " ++
    -- Pieces in this row
    concat [printSquare (Pos c r) board ++ " " | c <- ['a'..'h']] ++
    -- Row number on the right
    "| " ++ show r

-- Print a single square of the board
printSquare :: Position -> Map Position Piece -> String
printSquare pos board = case Map.lookup pos board of
    Nothing     -> "."  -- Empty square
    Just piece  -> pieceToUnicode piece

-- Display the current game state
displayGame :: GameState -> String
displayGame state =
    "\nCurrent player: " ++ show (currentPlayer state) ++ "\n" ++
    printBoard state ++ "\n" ++
    "Captured pieces: " ++ printCapturedPieces (capturedPieces state) ++ "\n" ++
    "Move history: " ++ show (length (moveHistory state)) ++ " moves\n" ++
    case gameResult state of
        Nothing -> "Game in progress"
        Just result -> "Game over: " ++ show result

-- Format captured pieces for display
printCapturedPieces :: [Piece] -> String
printCapturedPieces [] = "None"
printCapturedPieces pieces = unwords (map pieceToUnicode pieces)