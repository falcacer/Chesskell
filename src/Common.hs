{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ExistentialQuantification #-}

module Common where


-------------------------------------------------------------------------------

data Position = Pos Char Int deriving (Show, Eq, Ord)
data Color = White | Black deriving (Show, Eq)


class ChessPiece p where
    pieceName     :: p -> String
    pieceColor    :: p -> Color
    piecePosition :: p -> Position
    moveRule      :: p -> Position -> Bool
    captureRule   :: p -> Position -> Bool
    isClearPath   :: p -> Position -> [Piece] -> Bool
    toUnicode     :: p -> String


data King = King Color Position deriving (Show, Eq)
instance ChessPiece King where
    moveRule (King _ (Pos c1 r1)) (Pos c2 r2) = 
        let dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in  max dx dy == 1
    
    captureRule = moveRule
    
    isClearPath _ _ _ = True

    toUnicode (King Black _) = "♔"
    toUnicode (King White _) = "♚"
    
    pieceName _ = "King"
    
    pieceColor (King color _) = color

    piecePosition (King _ pos) = pos


data Queen = Queen Color Position deriving (Show, Eq)
instance ChessPiece Queen where
    moveRule (Queen _ (Pos c1 r1)) (Pos c2 r2) = 
        let dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in  dx == dy || dx == 0 || dy == 0
    
    captureRule = moveRule
    
    isClearPath (Queen _ (Pos c1 r1)) (Pos c2 r2) boardPieces = 
        let c1Int = fromEnum c1
            c2Int = fromEnum c2
            dx = abs (c2Int - c1Int)
            dy = abs (r2 - r1)
            
            stepX = if dx == 0 then 0 else (c2Int - c1Int) `div` dx
            stepY = if dy == 0 then 0 else (r2 - r1) `div` dy
            
            path = [Pos (toEnum (c1Int + i * stepX)) (r1 + i * stepY) | 
                   i <- [1..(max dx dy)-1]]
                   
            isPieceAt pos = any (\(Piece p) -> piecePosition p == pos) boardPieces
        in  not $ any isPieceAt path
        
    toUnicode (Queen Black _) = "♕"
    toUnicode (Queen White _) = "♛"
    
    pieceName _ = "Queen"
    
    pieceColor (Queen color _) = color

    piecePosition (Queen _ pos) = pos


data Rook = Rook Color Position deriving (Show, Eq)
instance ChessPiece Rook where
    moveRule (Rook _ (Pos c1 r1)) (Pos c2 r2) = 
        let dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in  dx == 0 || dy == 0

    captureRule = moveRule
        
    isClearPath (Rook _ (Pos c1 r1)) (Pos c2 r2) boardPieces = 
        let c1Int = fromEnum c1
            c2Int = fromEnum c2
            
            path = if r1 == r2
                then [Pos (toEnum c) r1 | c <- [(min c1Int c2Int + 1)..(max c1Int c2Int - 1)]]
                else [Pos c1 r | r <- [(min r1 r2 + 1)..(max r1 r2 - 1)]]

            isPieceAt pos = any (\(Piece p) -> piecePosition p == pos) boardPieces
        in  not $ any isPieceAt path

    toUnicode (Rook Black _) = "♖"
    toUnicode (Rook White _) = "♜"
    
    pieceName _ = "Rook"
    
    pieceColor (Rook color _) = color

    piecePosition (Rook _ pos) = pos


data Bishop = Bishop Color Position deriving (Show, Eq)
instance ChessPiece Bishop where
    moveRule (Bishop _ (Pos c1 r1)) (Pos c2 r2) = 
        let dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in  dx == dy
    
    captureRule = moveRule
        
    isClearPath (Bishop _ (Pos c1 r1)) (Pos c2 r2) boardPieces = 
        let c1Int = fromEnum c1
            c2Int = fromEnum c2
            
            stepX = signum (c2Int - c1Int)
            stepY = signum (r2 - r1)

            diagonalLength = abs (c2Int - c1Int) - 1
            
            path = [Pos (toEnum (c1Int + i * stepX)) (r1 + i * stepY) | 
                   i <- [1..diagonalLength]]
                   
            isPieceAt pos = any (\(Piece p) -> piecePosition p == pos) boardPieces
        in  not $ any isPieceAt path
    
    toUnicode (Bishop Black _) = "♗"
    toUnicode (Bishop White _) = "♝"
    
    pieceName _ = "Bishop"
    
    pieceColor (Bishop color _) = color

    piecePosition (Bishop _ pos) = pos


data Knight = Knight Color Position deriving (Show, Eq)
instance ChessPiece Knight where
    moveRule (Knight _ (Pos c1 r1)) (Pos c2 r2) =
        let dx = abs (fromEnum c2 - fromEnum c1)
            dy = abs (r2 - r1)
        in  (dx == 2 && dy == 1) || (dx == 1 && dy == 2)

    captureRule = moveRule
    
    isClearPath _ _ _ = True
    
    toUnicode (Knight Black _) = "♘"
    toUnicode (Knight White _) = "♞"
    
    pieceName _ = "Knight"
    
    pieceColor (Knight color _) = color

    piecePosition (Knight _ pos) = pos


data Pawn = Pawn Color Position deriving (Show, Eq)
instance ChessPiece Pawn where
    moveRule (Pawn color (Pos c1 r1)) (Pos c2 r2) =
        let dx = fromEnum c2 - fromEnum c1
            dy = r2 - r1
        in  if color == White
            then dx == 0 && (dy == 1 || (dy == 2 && r1 == 2))
            else dx == 0 && (dy == -1 || (dy == -2 && r1 == 7))

    captureRule (Pawn color (Pos c1 r1)) (Pos c2 r2) =
        let dx = abs (fromEnum c2 - fromEnum c1)
            dy = r2 - r1
        in  if color == White
            then dx == 1 && dy == 1
            else dx == 1 && dy == -1
    
    isClearPath (Pawn color (Pos c1 r1)) (Pos _ r2) boardPieces =
        if abs (r2 - r1) == 1
        then True
        else 
            let intermediateRow = if color == White then r1 + 1 else r1 - 1
                intermediatePos = Pos c1 intermediateRow
                isPieceAt pos = any (\(Piece p) -> piecePosition p == pos) boardPieces
            in not (isPieceAt intermediatePos)
        
    toUnicode (Pawn Black _) = "♙"
    toUnicode (Pawn White _) = "♟"
    
    pieceName _ = "Pawn"
    
    pieceColor (Pawn color _) = color

    piecePosition (Pawn _ pos) = pos


data Piece = forall p. (ChessPiece p, Eq p, Show p) => Piece p

instance Eq Piece where
    (Piece p1) == (Piece p2) = show p1 == show p2

instance Show Piece where
    show (Piece p) = pieceName p ++ " (" ++ show (pieceColor p) ++ "," ++ 
                    show (piecePosition p) ++ ")"


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
    board          :: [Piece],
    currentPlayer  :: Color,
    moveHistory    :: [Move],
    capturedPieces :: [Piece],
    inCheck        :: Bool,
    gameResult     :: Maybe GameResult
} deriving (Show, Eq)
