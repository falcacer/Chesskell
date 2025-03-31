module Monads where

import Common
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)


-- State monad to track game state through evaluations
newtype ChessState a = ChessState { runChess :: GameState -> Maybe (a, GameState) }

-- Functor instance for ChessState
instance Functor ChessState where
    fmap f (ChessState g) = ChessState $ \state -> 
        case g state of
            Nothing -> Nothing
            Just (a, newState) -> Just (f a, newState)
            
-- Applicative instance for ChessState
instance Applicative ChessState where
    pure a = ChessState $ \state -> Just (a, state)
    
    ChessState f <*> ChessState g = ChessState $ \state ->
        case f state of
            Nothing -> Nothing
            Just (h, state') -> case g state' of
                Nothing -> Nothing
                Just (a, state'') -> Just (h a, state'')

-- Monad instance for ChessState
instance Monad ChessState where
    return = pure
    
    ChessState m >>= f = ChessState $ \state ->
        case m state of
            Nothing -> Nothing
            Just (a, state') -> runChess (f a) state'

-- Helper function to get the current game state
getState :: ChessState GameState
getState = ChessState $ \state -> Just (state, state)

-- Helper function to modify the game state
modifyState :: (GameState -> GameState) -> ChessState ()
modifyState f = ChessState $ \state -> Just ((), f state)

-- Helper function to set the game state
setState :: GameState -> ChessState ()
setState newState = ChessState $ \_ -> Just ((), newState)

-- Run a chess game from an initial state
runGame :: ChessState a -> Maybe (a, GameState)
runGame state = runChess state initialGameState



