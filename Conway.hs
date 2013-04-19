module Conway where

import Data.Maybe

type Coord = (Int, Int)
data Cell = Cell { alive :: Bool
            , x :: Int
            , y :: Int
            } deriving (Eq)
type Row = [Cell]
type Field = [Row]

instance Show Cell where
    show (Cell alive x y) = if alive then "1" else "0"

nextState :: Field -> Field
nextState f = map (processRow f) rows
    where rows = f

processRow :: Field -> Row -> Row
processRow f r = map (processCell f) cells
    where cells = r

processCell :: Field -> Cell -> Cell
processCell f c
    | n < 2 = c {alive = False}
    | n == 2 = c
    | n == 3 = c {alive = True}
    | otherwise = c {alive = False}
    where neighbors = getNeighbors f c
          n = length $ filter alive neighbors

getCell :: Field -> Coord -> Maybe Cell
getCell f c = if inBounds f c then Just (f !! iy !! ix) else Nothing
    where ix = fst c
          iy = snd c

getNeighbors :: Field -> Cell -> [Cell]
getNeighbors f c = (getHoriz f c) ++ (getVert f c) ++ (getDiag f c)
    where ix = x c
          iy = y c

getDiag :: Field -> Cell -> [Cell]
getDiag f c = catMaybes [c1,c2,c3,c4]
    where ix = x c
          iy = y c
          c1 = getCell f ((ix-1),(iy-1))
          c2 = getCell f ((ix+1),(iy-1))
          c3 = getCell f ((ix-1),(iy+1))
          c4 = getCell f ((ix+1),(iy+1))

getVert :: Field -> Cell -> [Cell]
getVert f c = catMaybes [c1,c2]
    where ix = x c
          iy = y c
          c1 = getCell f (ix,iy-1)
          c2 = getCell f (ix,iy+1)

getHoriz :: Field -> Cell -> [Cell]
getHoriz f c = catMaybes [c1,c2]
    where ix = x c
          iy = y c
          c1 = getCell f (ix-1,iy)
          c2 = getCell f (ix+1,iy)

inBounds :: Field -> Coord -> Bool
inBounds f c = (xBounds w x) && (yBounds h y)
    where x = fst c
          y = snd c
          h = length f
          w = length $ head f
          xBounds w x
            | x < 0 = False
            | x >= w = False
            | otherwise = True
          yBounds h y
            | y < 0 = False
            | y >= h = False
            | otherwise = True
