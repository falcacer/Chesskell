module Monads where

import Common

-------------------------------------------------------------------------------

newtype ChessState a = ChessState { runChess :: GameState -> Maybe (a, GameState) }

instance Functor ChessState where
    fmap f (ChessState g) = ChessState $ \state -> 
        case g state of
            Nothing -> Nothing
            Just (a, newState) -> Just (f a, newState)
            

instance Applicative ChessState where
    pure a = ChessState $ \state -> Just (a, state)
    
    ChessState f <*> ChessState g = ChessState $ \state ->
        case f state of
            Nothing -> Nothing
            Just (h, state') -> case g state' of
                Nothing -> Nothing
                Just (a, state'') -> Just (h a, state'')

instance Monad ChessState where
    return = pure
    
    ChessState m >>= f = ChessState $ \state ->
        case m state of
            Nothing -> Nothing
            Just (a, state') -> runChess (f a) state'


getState :: ChessState GameState
getState = ChessState $ \state -> Just (state, state)

modifyState :: (GameState -> GameState) -> ChessState ()
modifyState f = ChessState $ \state -> Just ((), f state)

setState :: GameState -> ChessState ()
setState newState = ChessState $ \_ -> Just ((), newState)

runGame :: ChessState a -> GameState ->  Maybe (a, GameState)
runGame state game = runChess state game
