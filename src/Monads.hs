{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Monads where

import Common

-------------------------------------------------------------------------------

data ChessError = 
      InvalidMove Piece Position String
    | BlockedPath Piece Position [Position]
    | WrongTurn Piece Color
    | PieceNotFound Position
    | MoveIntoCheck Piece Position
    | SelfCapture Piece Position
    | GameOver GameResult
    deriving (Show, Eq)

newtype ChessState a = ChessState { runChess :: GameState -> Either ChessError (a, GameState) }

instance Functor ChessState where
    fmap f (ChessState g) = ChessState $ \state -> 
        case g state of
            Left err -> Left err
            Right (a, newState) -> Right (f a, newState)
            
instance Applicative ChessState where
    pure a = ChessState $ \state -> Right (a, state)
    
    ChessState f <*> ChessState g = ChessState $ \state ->
        case f state of
            Left err -> Left err
            Right (h, state') -> case g state' of
                Left err -> Left err
                Right (a, state'') -> Right (h a, state'')

instance Monad ChessState where
    return = pure
    
    ChessState m >>= f = ChessState $ \state ->
        case m state of
            Left err -> Left err
            Right (a, state') -> runChess (f a) state'

getState :: ChessState GameState
getState = ChessState $ \state -> Right (state, state)

modifyState :: (GameState -> GameState) -> ChessState ()
modifyState f = ChessState $ \state -> Right ((), f state)

setState :: GameState -> ChessState ()
setState newState = ChessState $ \_ -> Right ((), newState)

runGame :: ChessState a -> GameState -> Either ChessError (a, GameState)
runGame state game = runChess state game

throwError :: ChessError -> ChessState a
throwError err = ChessState $ \_ -> Left err

invalidMoveError :: Piece -> Position -> String -> ChessState a
invalidMoveError piece pos reason = throwError $ InvalidMove piece pos reason

blockedPathError :: Piece -> Position -> [Position] -> ChessState a
blockedPathError piece pos blockedPositions = throwError $ BlockedPath piece pos blockedPositions

wrongTurnError :: Piece -> Color -> ChessState a
wrongTurnError piece color = throwError $ WrongTurn piece color

moveIntoCheckError :: Piece -> Position -> ChessState a
moveIntoCheckError piece pos = throwError $ MoveIntoCheck piece pos

selfCaptureError :: Piece -> Position -> ChessState a
selfCaptureError piece pos = throwError $ SelfCapture piece pos

pieceNotFoundError :: Position -> ChessState a
pieceNotFoundError pos = throwError $ PieceNotFound pos

gameOverError :: GameResult -> ChessState a
gameOverError result = throwError $ GameOver result