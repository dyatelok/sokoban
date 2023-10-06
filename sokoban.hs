{-# LANGUAGE OverloadedStrings #-}

module Main where

import CodeWorld
import Data.List

wall :: Picture
wall = colored gray (solidRectangle 1 1)

ground :: Picture
ground = colored yellow (solidRectangle 1 1)

storage :: Picture
storage = solidCircle 0.25 & colored yellow (solidRectangle 1 1)

box :: Picture
box = colored orange (solidRectangle 1 1)

data Tile = Blank | Wall | Ground | Storage | Box deriving (Eq)

newtype Pos = Pos Int

instance Eq Pos where
  (==) (Pos a) (Pos b) = a == b

instance Ord Pos where
  (<=) (Pos a) (Pos b) = a <= b

drawTile :: Tile -> Picture
drawTile Blank = blank
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box

setTile :: Int -> Tile -> Picture
setTile t m = translated (fromIntegral (decodeX t) :: Double) (fromIntegral (decodeY t) :: Double) (drawTile m)

setXYTile :: Int -> Int -> Tile -> Picture
setXYTile x y t = translated (fromIntegral x :: Double) (fromIntegral y :: Double) (drawTile t)

player :: Direction -> Picture
player dir = case dir of
  U -> rotated 0 plr
  D -> rotated pi plr
  L -> rotated (pi / 2) plr
  R -> rotated (- pi / 2) plr
  where
    plr = translated 0 0.2 (solidRectangle 0.2 0.2 & colored white (solidRectangle 0.4 0.4)) & colored red $ solidRectangle 0.8 0.8

playerPicture :: State -> Picture
playerPicture (State d (Pos t) _) = translated (fromIntegral (decodeX t) :: Double) (fromIntegral (decodeY t) :: Double) (player d)

maze :: Int -> Int -> Tile
maze x y
  | abs x > 10 || abs y > 10 = Blank
  | x == 0 && y == 0 = Ground
  | abs x == 9 && abs y == 9 = Wall
  | abs x == 10 || abs y == 10 = Wall
  | x == y = Storage
  | abs x == abs y = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0 = Storage
  | otherwise = Ground

mazeNoBoxes :: Int -> Int -> Tile
mazeNoBoxes x y = case tile of
  Box -> Ground
  _ -> tile
  where 
  tile = maze x y

isBoxInBase :: Int -> Bool
isBoxInBase t = case maze (decodeX t) (decodeY t) of
  Box -> True
  _ -> False

isStorageInBase :: Int -> Bool
isStorageInBase t = case maze (decodeX t) (decodeY t) of
  Storage -> True
  _ -> False

mazeOnlyBoxes :: [Pos] -> Int -> Int -> [Pos]
mazeOnlyBoxes acc t m
  | t >= m = acc
  | t < m = if isBoxInBase t then Pos t : next else next
  | otherwise = []
  where
  next = mazeOnlyBoxes acc (t+1) m

mazeOnlyStorages :: [Pos] -> Int -> Int -> [Pos]
mazeOnlyStorages acc t m
  | t >= m = acc
  | t < m = if isStorageInBase t then Pos t : next else next
  | otherwise = []
  where
  next = mazeOnlyStorages acc (t+1) m

decodeX :: Int -> Int
decodeY :: Int -> Int
decodeX t = t `div` 21 - 10

decodeY t = t `mod` 21 - 10

whichTile t = mazeNoBoxes (decodeX t) (decodeY t)

mazz :: Picture -> Int -> Int -> Picture
mazz acc t m
  | t >= m = acc
  | t < m = mazz (setTile t (whichTile t) & acc) (t + 1) m
  | otherwise = blank

pictureOfMaze :: Picture
pictureOfMaze = mazz blank 0 (21 * 21)

boxesPicture :: [Pos] -> Picture
boxesPicture [] = blank
boxesPicture ((Pos x) : bx) = setTile x Box & boxesPicture bx

newtype Boxes = Boxes [Pos]

data State = State Direction Pos Boxes

data Direction = U | D | L | R

canMove :: Pos -> Bool
canMove (Pos t) = tl == Storage || tl == Ground || tl == Box
  where
    tl = maze (decodeX t) (decodeY t)

isBox :: [Pos] -> Pos -> Bool
isBox [] _ = False
isBox (x : bx) t = x == t || isBox bx t

movBox :: [Pos] -> Pos -> Direction -> [Pos]
movBox [x] _ dir = [dirPos dir x]
movBox (x: bx) b dir = if x == b then dirPos dir x : bx else x : movBox bx b dir


movIfCan :: Direction -> State -> (Pos,Boxes)
movIfCan dir (State _ p bbx @ (Boxes bx))
  | isBox bx nPos  = if canMove nnPos && not (isBox bx nnPos) then (nPos, Boxes (movBox bx nPos dir)) else (p,bbx)
  | canMove  nPos = (dirPos dir p,bbx) 
  | otherwise = (p,bbx)
  where
  nPos = dirPos dir p
  nnPos = dirPos dir nPos

dirPos :: Direction -> Pos -> Pos
dirPos dir (Pos p) = Pos (p + dirToAdd dir)

dirToAdd :: Direction -> Int
dirToAdd dir = case dir of
  U -> 1
  D -> (-1)
  L -> (-21)
  R -> 21

move :: Direction -> State -> State
move dir s = uncurry (State dir) new
  where
  new = movIfCan dir s

isWon :: State -> Bool
isWon (State _ _ (Boxes bx)) = sort bx == sort (mazeOnlyStorages [] 0 (21 * 21))

resetableActivityOf :: world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resetableActivityOf initialState eventHandler = activityOf initialState eventHandlerMod
  where
    eventHandlerMod event state =
      case event of
        KeyPress "Esc" -> initialState
        _ -> eventHandler event state

main :: IO ()
main = resetableActivityOf initialState handleEvent draw
  where
    initialState :: State
    initialState = State U (Pos 161) (Boxes (mazeOnlyBoxes [] 0 (21 * 21)))

    draw state @ (State _ _ (Boxes bx)) = if isWon state then lettering "You won!" else playerPicture state & boxesPicture bx & pictureOfMaze

    handleEvent event state =
      case event of
        KeyPress "Up" -> move U state
        KeyPress "Down" -> move D state
        KeyPress "Left" -> move L state
        KeyPress "Right" -> move R state
        _ -> state
