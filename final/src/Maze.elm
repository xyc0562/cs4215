module MazeGen where

import Native.Random

random : Float
random = Native.Random.random()

-- Walls surround a cell
type Wall = Intact | Breached

-- A cell is nothing more than an index
type alias Cell = (Int, Int)

type alias Maze =
