{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib2(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
    ( Document(DInteger, DList, DNull, DMap), Coord(..), Check(..) )

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State =
  Cord [(Int,Int,Char)]
  | Test [String]
  deriving (Show, Eq)
-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState  = (Cord [])

-- IMPLEMENT
-- This adds game data to initial state 

gameStart :: State -> Document -> Either String State
gameStart (Cord state) doc = 
        case makeCords doc ([0],[0]) of
            Left _ -> Left "Error: Cannot return coordinates "
            Right cords -> 
                case makeTuple cords 'x' [] of
                    Left _ -> Left "Error: Cannot return tuple "
                    Right tuple -> Right (Cord (state++tuple)) 
gameStart _ _ = Left "Error: Wrong parameters "

makeCords :: Document -> ([Int],[Int]) -> Either String ([Int],[Int])
makeCords (DMap ((str,doc):t)) (col,row) = 
    case str of
        "number_of_hints" -> (makeCords  (DMap t)  (col,row)) 
        "occupied_cols"-> 
            case makeList doc [] of
                Left _ -> Left "Error: Cannot return list "
                Right cols -> (makeCords  (DMap t) (cols,row)) 
        "occupied_rows"-> 
            case makeList doc [] of 
                Left _ -> Left "Error: Cannot return list "
                Right rows -> Right (col,rows) 
        _ -> Left (show str)   
makeCords _ _ = Left "Error: Wrong parameters "

makeList  :: Document ->[Int]-> Either String [Int]
makeList d list = 
    case d of
        (DList [DInteger last]) -> Right (last:list)   
        (DList (h:t)) -> 
            case parser_DInteger h of
                Left _ -> Left "Error: Wrong first parameter"
                Right int -> (makeList (DList (t)) (int:list))
        _ -> Left (show "Error: Wrong first parameter ") 
makeList _ _ = Left "Error: Wrong parameters "

makeTuple  :: ([Int],[Int])->Char->[(Int,Int,Char)]-> Either String [(Int,Int,Char)]
makeTuple x char tuple =
    case x of
        ((hC:tC),(hR:tR)) -> (makeTuple (tC,tR) char ((hC,hR,char):tuple)) 
        ([],[]) -> Right (tuple)
        _ -> Left "Error: Bad parametres " 
makeTuple _ _ _ = Left "Error: Wrong parametres "

render :: State->String
render (Cord st)= makeSpace (render_ (sort st []) (0,0,10,10) "") ""

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (Cord st) = 
    let miss = findElements_byChar st '@' []
        good = findElements_byChar st '!' []
        coord = fromListToCoord (miss++good) []
    in ( Check {coords = coord})

fromListToCoord :: [(Int, Int, Char)] -> [Coord] ->[Coord]
fromListToCoord ((x,y,c):t) coords = fromListToCoord t ((Coord  {col = x, row = y}): coords)
fromListToCoord end rez = rez

findElements_byChar :: [(Int, Int, Char)]->(Char)->[(Int, Int, Char)]->[(Int, Int, Char)]
findElements_byChar ((c,r,char):t) looking rez
    | char==looking =findElements_byChar t  looking ((c,r,char):rez)
    | otherwise = findElements_byChar t  looking (rez)
findElements_byChar end looking rez = rez

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle l t = toggle_ l t

toggle_ :: State -> [String]->State
toggle_ (Cord st) (hh:ht:tt) = 
    let colInt = takeInt hh
        rowInt = takeInt ht
        char = findElement st (colInt,rowInt,'x')
    in toggle_ (Cord ((colInt,rowInt,char):st)) tt
toggle_ a b= a

findElement :: [(Int, Int, Char)]->(Int, Int, Char)->Char
findElement (h:t) looking 
    | looking==h =  '!'
    | otherwise = findElement t looking
findElement end max ='@'


takeInt :: String -> Int
takeInt str = read str :: Int
-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> Either String State
hint l h = (hintf h l (0,0,'x'))
hint _ _ =  Left "Error: Wrong parameters"

hintf :: Document -> State ->(Int,Int,Char)-> Either String State
hintf (DNull) (Cord state2) (x,y,c) = Right (Cord state2)
hintf (DMap ((str,doc):t)) (Cord state2) (x,y,c) =
    case str of
        "coords"-> hintf doc (Cord state2) (x,y,c)
        "tail"-> hintf doc (Cord state2) (x,y,c)
        "head"-> 
            case hintf doc (Cord state2) (x,y,c) of
                Left _ -> Left "Error: Wrong second parameter "
                Right state -> hintf (DMap t) (state) (x,y,c)
        "col"-> 
            case (parser_DInteger doc) of
                Left _ -> Left "Error: Wrong first parameter "
                Right int -> hintf (DMap t) (Cord state2) (int ,y,'o')
        "row"->  
            case (parser_DInteger doc) of
                Left _ -> Left "Error: Wrong first parameter "
                Right int -> Right (Cord ((x,int,c): state2))
        _ -> Left "Error: Wrong string "
hintf _ _ _ = Left "Error: Wrong parameters "

parser_DInteger :: Document -> Either String Int
parser_DInteger (DInteger a) = Right a   
parser_DInteger a = Left "its not DInteger" 


sortList :: [(Int, Int, Char)] -> [(Int, Int, Char)]
sortList (z:zs) = if (head zs) < z then (z:zs) else (z:(sortList zs))

findMax :: [(Int, Int, Char)]->(Int, Int, Char)->(Int, Int, Char)
findMax (h:t) max 
    | h>max =  findMax t h
    | otherwise = findMax t max
findMax end max =max

sort :: [(Int, Int, Char)]->[(Int, Int, Char)]->[(Int, Int, Char)]
sort (h:t) newList =
    let max = findMax (h:t) h
        removedList= remove_element (h:t) [] max
    in sort removedList (max:newList) 
sort [] newList = newList

remove_element:: [(Int, Int, Char)]-> [(Int, Int, Char)]-> (Int, Int, Char)-> [(Int, Int, Char)]
remove_element (h:t) newList value 
    | h==value = newList++t
    | otherwise = remove_element t (h:newList) value


render_ :: [(Int,Int,Char)]-> (Int,Int,Int,Int)->String->String
render_ ((objx,objy,objc):t) (x,y,width,hight) rez
    | y>width = render_ ((objx,objy,objc):t) (x+1,0,width,hight) ('|':rez)
    | x>hight = error (show rez)
    | objx == x && objy == y && objc == 'x' =  render_ t (x,y+1,width,hight) ('=':rez)
    | objx == x && objy == y =  render_ t (x,y+1,width,hight) (objc:rez) 
    | objy > y || objx > x   = render_ ((objx,objy,objc):t) (x,y+1,width,hight) ('=':rez)
    | otherwise = render_ t (x,y,width,hight) rez
render_ [] (x,y,width,hight) rez 
    | y>width = render_ [] (x+1,0,width,hight) ('|':rez)
    | x>hight = rez
    | otherwise =render_ [] (x,y+1,width,hight) ('=':rez) 
    
makeSpace ::  String -> String -> String
makeSpace (h : t) rez =
  case h of
    '|' -> makeSpace t ('\n' : rez)
    _ -> makeSpace t (h : rez)
makeSpace "" rez = rez