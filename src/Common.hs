{-# LANGUAGE ExistentialQuantification #-}

module Common where

-------------------------------------------------------------------------------
data Position = Pos Char Int deriving (Show, Eq, Ord)
data Color = White | Black deriving (Show, Eq)

class ChessPiece p where
    move :: p -> Position ->  Bool
    capture :: p -> Position -> Bool
    pathIsClear :: p -> Position -> Bool
    toUnicode :: p -> String
    pieceName :: p -> String
    pieceColor :: p -> Color
    piecePosition :: p -> Position

data King = King Color Position deriving (Show, Eq)
instance ChessPiece King where
    move (King _ (Pos c1 r1)) (Pos c2 r2) = 
        let 
            dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in max dx dy == 1
    
    capture = move
    
    pathIsClear _ _ = True

    toUnicode (King Black _) = "♔"
    toUnicode (King White _) = "♚"
    
    pieceName _ = "King"
    
    pieceColor (King color _) = color

    piecePosition (King _ pos) = pos


data Queen = Queen Color Position deriving (Show, Eq)
instance ChessPiece Queen where
    move (Queen _ (Pos c1 r1)) (Pos c2 r2) = 
        let
            dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in dx == dy || dx == 0 || dy == 0
    
    capture = move
    
    pathIsClear _ _ =  True
    
    toUnicode (Queen Black _) = "♕"
    toUnicode (Queen White _) = "♛"
    
    pieceName _ = "Queen"
    
    pieceColor (Queen color _) = color

    piecePosition (Queen _ pos) = pos


data Rook = Rook Color Position deriving (Show, Eq)
instance ChessPiece Rook where
    move (Rook _ (Pos c1 r1)) (Pos c2 r2) = 
        let
            dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in dx == 0 || dy == 0

    capture = move
        
    pathIsClear _ _ =  True

    toUnicode (Rook Black _) = "♖"
    toUnicode (Rook White _) = "♜"
    
    pieceName _ = "Rook"
    
    pieceColor (Rook color _) = color

    piecePosition (Rook _ pos) = pos

data Bishop = Bishop Color Position deriving (Show, Eq)
instance ChessPiece Bishop where
    move (Bishop _ (Pos c1 r1)) (Pos c2 r2) = 
        let
            dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in dx == dy
    
    capture = move
        
    pathIsClear _ _ =  True
    
    toUnicode (Bishop Black _) = "♗"
    toUnicode (Bishop White _) = "♝"
    
    pieceName _ = "Bishop"
    
    pieceColor (Bishop color _) = color

    piecePosition (Bishop _ pos) = pos


data Knight = Knight Color Position deriving (Show, Eq)
instance ChessPiece Knight where
    move (Knight _ (Pos c1 r1)) (Pos c2 r2) =
        let
            dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in (dx == 2 && dy == 1) || (dx == 1 && dy == 2)

    capture = move
    
    pathIsClear _ _ =  True
    
    toUnicode (Knight Black _) = "♘"
    toUnicode (Knight White _) = "♞"
    
    pieceName _ = "Knight"
    
    pieceColor (Knight color _) = color

    piecePosition (Knight _ pos) = pos


data Pawn = Pawn Color Position deriving (Show, Eq)
instance ChessPiece Pawn where
    move (Pawn color (Pos c1 r1)) (Pos c2 r2) =
        let
            dx = fromEnum c2 - fromEnum c1
            dy = r2 - r1
        in if color == White
           then dx == 0 && (dy == 1 || (dy == 2 && r1 == 2))
           else dx == 0 && (dy == -1 || (dy == -2 && r1 == 7))

    capture (Pawn color (Pos c1 r1)) (Pos c2 r2) =
        let
            dx = abs (fromEnum c2 - fromEnum c1)
            dy = r2 - r1
        in if color == White
           then dx == 1 && dy == 1
           else dx == 1 && dy == -1
    
    pathIsClear _ _ =  True
        
    toUnicode (Pawn Black _) = "♙"
    toUnicode (Pawn White _) = "♟"
    
    pieceName _ = "Pawn"
    
    pieceColor (Pawn color _) = color

    piecePosition (Pawn _ pos) = pos


data Piece = forall p. (ChessPiece p, Eq p, Show p) => Piece p

instance Eq Piece where
    (Piece p1) == (Piece p2) = show p1 == show p2

instance Show Piece where
    show (Piece p) = pieceName p ++ " (" ++ show (pieceColor p) ++ "," ++ show (piecePosition p) ++ ")"


data CastleType = KingSide | QueenSide deriving (Show, Eq)

data PromotionPiece = PromoteToQueen | PromoteToRook | PromoteToBishop | PromoteToKnight
    deriving (Show, Eq)

data MoveType = Normal 
                | Capture
                | Promotion PromotionPiece
                deriving (Show, Eq)

data Move = Move Piece MoveType Position deriving (Show, Eq)

type Moves = [Move]

data GameResult = WhiteWin | BlackWin | Draw deriving (Show, Eq)

data GameState = GameState {
    board :: [Piece], 
    currentPlayer :: Color,
    moveHistory :: [Move],
    capturedPieces :: [Piece],
    inCheck :: Bool,
    gameResult :: Maybe GameResult
} deriving (Show, Eq)
