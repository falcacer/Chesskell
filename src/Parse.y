{
module Parse where

import Common
import Utils
import Data.Char (isDigit, digitToInt)
}
%monad { P } { thenP } { returnP }
%name parse
%error { parseError }

%tokentype { Token }

%token
  piece { TPiece $$ }
  col { TCol $$ }
  row { TRow $$ }
  '-' { TDash }
  '>' { TArrow }
  'x' { TCapture }
  ';' { TSemicolon }
  '=' { TEqual }
  EOF { TEOF }
%%

-- Add a top-level rule to handle the entire input
game :: { Moves }
game : move_sequence EOF { $1 }

move_sequence :: { Moves }
move_sequence : move { [$1] }
              | move ';' move_sequence { $1 : $3 }

move :: { Move }
move : piece '-' position '>' position {
          let piece = charToPiece $1 $3
          in Move piece Normal $5
      }
     | piece 'x' position '>' position { 
          let piece = charToPiece $1 $3
          in Move piece Capture $5 
      }
     | piece '-' position '=' piece { 
          let 
              pawn = charToPiece $1 $3
              promotionPiece = pieceNameToPromotionPiece $5
          in Move pawn (Promotion promotionPiece) $3
        }

position :: { Position }
position : col row { Pos $1 (digitToInt $2) }

{

data P a = Ok a | Failed String
                    deriving Show

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = case m of
                Ok a     -> k a
                Failed e -> Failed e

returnP :: a -> P a
returnP a = Ok a

failP :: String -> P a
failP err = Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = case m of
                        Ok a     -> Ok a
                        Failed e -> k e

parseError :: [Token] -> P a
parseError [] = failP "Unexpected end of input"
parseError (t:ts) = failP $ "Parse error at token: " ++ show t


data Token = TPiece Char
           | TCol Char
           | TRow Char
           | TDash
           | TArrow
           | TCapture
           | TSemicolon
           | TEqual
           | TEOF
           deriving (Show)

-- Modify lexer to add an EOF token
lexer :: String -> [Token]
lexer cs = let tokens = lexer' cs in tokens ++ [TEOF]
  where
    lexer' [] = []
    lexer' ('\n':cs) = lexer' cs
    lexer' ('\r':cs) = lexer' cs
    lexer' (' ':cs) = lexer' cs
    lexer' ('-':cs) = TDash : lexer' cs
    lexer' ('>':cs) = TArrow : lexer' cs
    lexer' ('x':cs) = TCapture : lexer' cs
    lexer' (';':cs) = TSemicolon : lexer' cs
    lexer' ('=':cs) = TEqual : lexer' cs
    lexer' (c:cs)
        | c `elem` ['1'..'8'] = TRow c : lexer' cs
        | c `elem` ['a'..'h'] = TCol c : lexer' cs
        | c `elem` ['K', 'Q', 'R', 'S', 'N', 'P', 'k', 'q', 'r', 's', 'n', 'p'] = TPiece c : lexer' cs
        | otherwise = error $ "Unexpected character: " ++ [c]

}