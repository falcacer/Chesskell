module PrettyPrint where

import Common
import Utils

pieceToUnicode :: Piece -> String
pieceToUnicode piece = extractChessPiece piece toUnicode

printBoard :: GameState -> String
printBoard state = unlines (
    "    a b c d e f g h    " :
    "  +-----------------+  " :
    [printRow r (board state) | r <- [8,7..1]] ++
    ["  +-----------------+  ",
     "    a b c d e f g h    "])

printRow :: Int -> [Piece] -> String
printRow r pieces =
    show r ++ " | " ++
    concat [printSquare (Pos c r) pieces ++ " " | c <- ['a'..'h']] ++
    "| " ++ show r

printSquare :: Position -> [Piece] -> String
printSquare pos pieces = 
    case getPieceAtPosition pos pieces of
        Nothing -> "."
        Just piece -> pieceToUnicode piece


displayGame :: GameState -> String
displayGame state =
    "\nCurrent player: " ++ show (currentPlayer state) ++ "\n" ++
    printBoard state ++ "\n" ++
    "Captured pieces: " ++ printCapturedPieces (capturedPieces state) ++ "\n" ++
    "Move history: " ++ show (length (moveHistory state)) ++ " moves\n" ++
    case gameResult state of
        Nothing -> "Game in progress"
        Just result -> "Game over: " ++ show result

printCapturedPieces :: [Piece] -> String
printCapturedPieces [] = "None"
printCapturedPieces pieces = unwords (map pieceToUnicode pieces)