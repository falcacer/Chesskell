{-# LANGUAGE ExistentialQuantification #-}

module Common where

-------------------------------------------------------------------------------
data Position = Pos Char Int deriving (Show, Eq, Ord)
data Color = White | Black deriving (Show, Eq)

class ChessPiece p where
    isValidMove :: p -> Position ->  Bool
    pathIsClear :: p -> Position -> Bool
    toUnicode :: p -> String
    pieceName :: p -> String
    pieceColor :: p -> Color
    piecePosition :: p -> Position
    updatePosition :: Position -> p -> p

data King = King Color Position deriving (Show, Eq)
instance ChessPiece King where
    isValidMove (King _ (Pos c1 r1)) (Pos c2 r2) = let 
        dx = abs (fromEnum c2 - fromEnum c1)
        dy = abs (r2 - r1)
        in max dx dy == 1
    
    pathIsClear _ _ = True

    toUnicode (King Black _) = "♔"
    toUnicode (King White _) = "♚"
    
    pieceName _ = "King"
    
    pieceColor (King color _) = color

    piecePosition (King _ pos) = pos

    updatePosition newPos (King color _) = King color newPos


data Queen = Queen Color Position deriving (Show, Eq)
instance ChessPiece Queen where
    isValidMove (Queen _ (Pos c1 r1)) (Pos c2 r2) = let
        dx = abs (fromEnum c2 - fromEnum c1)
        dy = abs (r2 - r1)
        in dx == dy || dx == 0 || dy == 0
    
    pathIsClear _ _ =  True
    
    toUnicode (Queen Black _) = "♕"
    toUnicode (Queen White _) = "♛"
    
    pieceName _ = "Queen"
    
    pieceColor (Queen color _) = color

    piecePosition (Queen _ pos) = pos

    updatePosition newPos (Queen color _) = Queen color newPos

data Rook = Rook Color Position deriving (Show, Eq)
instance ChessPiece Rook where
    isValidMove (Rook _ (Pos c1 r1)) (Pos c2 r2) = let
        dx = abs (fromEnum c2 - fromEnum c1)
        dy = abs (r2 - r1)
        in dx == 0 || dy == 0
        
    pathIsClear _ _ =  True
    toUnicode (Rook Black _) = "♖"
    toUnicode (Rook White _) = "♜"
    
    pieceName _ = "Rook"
    
    pieceColor (Rook color _) = color

    piecePosition (Rook _ pos) = pos

    updatePosition newPos (Rook color _) = Rook color newPos

data Bishop = Bishop Color Position deriving (Show, Eq)
instance ChessPiece Bishop where
    isValidMove (Bishop _ (Pos c1 r1)) (Pos c2 r2) = let
        dx = abs (fromEnum c2 - fromEnum c1)
        dy = abs (r2 - r1)
        in dx == dy
        
    pathIsClear _ _ =  True
    
    toUnicode (Bishop Black _) = "♗"
    toUnicode (Bishop White _) = "♝"
    
    pieceName _ = "Bishop"
    
    pieceColor (Bishop color _) = color

    piecePosition (Bishop _ pos) = pos

    updatePosition newPos (Bishop color _) = Bishop color newPos

data Knight = Knight Color Position deriving (Show, Eq)
instance ChessPiece Knight where
    isValidMove (Knight _ (Pos c1 r1)) (Pos c2 r2) =  let
        dx = abs (fromEnum c2 - fromEnum c1)
        dy = abs (r2 - r1)
        in (dx == 2 && dy == 1) || (dx == 1 && dy == 2)
    
    pathIsClear _ _ =  True
    
    toUnicode (Knight Black _) = "♘"
    toUnicode (Knight White _) = "♞"
    
    pieceName _ = "Knight"
    
    pieceColor (Knight color _) = color

    piecePosition (Knight _ pos) = pos

    updatePosition newPos (Knight color _) = Knight color newPos

data Pawn = Pawn Color Position deriving (Show, Eq)
instance ChessPiece Pawn where
    isValidMove (Pawn color (Pos c1 r1)) (Pos c2 r2) =  let
        dx = fromEnum c2 - fromEnum c1
        dy = r2 - r1
        in if color == White
           then dx == 0 && (dy == 1 || (dy == 2 && r1 == 2))
           else dx == 0 && (dy == -1 || (dy == 2 && r1 == 7))
               
    
    pathIsClear _ _ =  True
        
    toUnicode (Pawn Black _) = "♙"
    toUnicode (Pawn White _) = "♟"
    
    pieceName _ = "Pawn"
    
    pieceColor (Pawn color _) = color

    piecePosition (Pawn _ pos) = pos

    updatePosition newPos (Pawn color _) = Pawn color newPos

data Piece = forall p. (ChessPiece p, Eq p, Show p) => Piece p

instance Eq Piece where
    (Piece p1) == (Piece p2) = show p1 == show p2

instance Show Piece where
    show (Piece p) = pieceName p ++ " (" ++ show (pieceColor p) ++ ")"


data CastleType = KingSide | QueenSide deriving (Show, Eq)

data PromotionPiece = PromoteToQueen | PromoteToRook | PromoteToBishop | PromoteToKnight
    deriving (Show, Eq)

data MoveType = Normal 
                | Capture
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
