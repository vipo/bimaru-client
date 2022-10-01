{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State =
  List [State] |
  Hints Int    |
  Cols [Int]   | 
  Rows [Int]
  deriving (Show, Eq)

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState  = List []

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart state doc = parser doc [state]


render :: State -> String
render = show

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle l t = emptyState

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint l h = emptyState

parser :: Document -> [State] -> State
parser (DMap((a, b):t)) state = 
    case a of 
    "number_of_hints" -> parser (DMap t) (Hints   (parserDInteger b):state)
    "occupied_cols" -> parser (DMap t) (Cols   (colTran b []):state)
    "occupied_rows" -> parser (DMap t) (Rows   (colTran b []):state)
    _ -> List state
parser doc st = List st 


colTran :: Document -> [Int] -> [Int]
colTran (DList (skaicius :t)) listas= colTran (DList t) (parserDInteger skaicius:listas) 
colTran nulis listas = listas


parserDInteger :: Document -> Int
parserDInteger (DInteger a) = a
parserDInteger a = error "its not DInteger"
