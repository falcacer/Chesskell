{
module Parse where

import Common
import Data.Char (isDigit, digitToInt)
}

%name parseChess
%tokentype { Token }
%error { parseError }

%token
  piece { TPiece $$ }
  col { TCol $$ }
  row { TRow $$ }
  '-' { TDash }
  'x' { TCapture }
  ';' { TSemicolon }
%%

game :: { Game }
game : move_sequence { Game $1 }

move_sequence :: { [Move] }
move_sequence : move { [$1] }
              | move ';' move_sequence { $1 : $3 }

move :: { Move }
move : piece '-' position { Move (MkPiece (pieceFromChar $1) (Info 0 (Pos 'a' 7))) Normal $3 }
     | piece 'x' position { Move (MkPiece (pieceFromChar $1) (Info 0 (Pos 'a' 7))) Capture $3 }

position :: { Position }
position : col row { Pos $1 (digitToInt $2) }

{

parseError :: [Token] -> a
parseError [] = error "Unexpected end of input"
parseError (t:_) = error $ "Parse error at token: " ++ show t

data Token = TPiece Char
           | TCol Char
           | TRow Char
           | TDash
           | TCapture
           | TSemicolon
           | TEOF
           deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('\n':cs) = lexer cs
lexer ('\r':cs) = lexer cs
lexer (' ':cs) = lexer cs
lexer ('-':cs) = TDash : lexer cs
lexer ('x':cs) = TCapture : lexer cs
lexer (';':cs) = TSemicolon : lexer cs
lexer (c:cs) 
    | c `elem` ['1'..'8'] = TRow c : lexer cs
    | c `elem` ['a'..'h'] = TCol c : lexer cs
    | c `elem` ['K', 'Q', 'R', 'B', 'N', 'P'] = TPiece c : lexer cs
    | otherwise = error $ "Unexpected character: " ++ [c]

parser :: String -> Game
parser = parseChess . lexer
}