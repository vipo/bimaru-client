{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the name
data State = State [String]
    deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State ["Initial state"]

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: Document -> State -> State
gameStart d (State l) = State $ ("Game started: " ++ show d) : l

-- IMPLEMENT
-- renders your game board
render :: State -> String
render = show

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []