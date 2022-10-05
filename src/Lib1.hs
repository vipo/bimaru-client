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
  Cord [(Int,Int,Char)]
  deriving (Show, Eq)
-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState  = (Cord [])

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (Cord state) doc =
    let cords=makeCords doc ([0],[0]) 
        tuple= makeTuple cords 'x' []
    in Cord (tuple++state)
makeCords :: Document -> ([Int],[Int]) -> ([Int],[Int])
makeCords (DMap ((str,doc):t)) (col,row) =
    case str of
        "number_of_hints"-> makeCords  (DMap t) (col,row)
        "occupied_cols"-> makeCords  (DMap t) ((makeList doc []),row)
        "occupied_rows"-> (col,(makeList doc []))
        _->  error (show str)

makeList  :: Document ->[Int]->[Int]
makeList (DList [DInteger last]) list= last:list
makeList (DList (h:t)) list =makeList (DList (t)) ((parser_DInteger h):list)
makeList dox a = error (show "Bad parrametres")

makeTuple  :: ([Int],[Int])->Char->[(Int,Int,Char)]->[(Int,Int,Char)]
makeTuple ((hC:tC),(hR:tR)) char tuple = makeTuple (tC,tR) char ((hC,hR,char):tuple)
makeTuple ([],[]) char tuple = tuple
makeTuple e1 e2 e3 = error "Bad parametres"

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
hint l h = hintf h l (0,0,'x')

hintf :: Document -> State ->(Int,Int,Char)-> State
hintf (DNull) (Cord state2) (x,y,c)= (Cord state2)
hintf (DMap ((str,doc):t)) (Cord state2) (x,y,c)=
    case str of
        "coords"-> hintf doc (Cord state2) (x,y,c)
        "tail"-> hintf doc (Cord state2) (x,y,c)
        "head"-> hintf (DMap t) (hintf doc (Cord state2) (x,y,c)) (x,y,c)
        "col"-> hintf (DMap t) (Cord state2) ((parser_DInteger doc),y,'o')
        "row"->  Cord ((x,(parser_DInteger doc),c): state2)

parser_DInteger :: Document -> Int
parser_DInteger (DInteger a) = a
parser_DInteger a = error "its not DInteger"