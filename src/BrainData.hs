module BrainData (
  BrainfuckCommand(..),
  Code,
  Tape(..),
  emptyTape8,
  emptyTape16) where

import Foundation

type Code = [BrainfuckCommand]

data BrainfuckCommand = GoRight Int    -- >
                      | GoLeft Int    -- <
                      | Increment Int -- +
                      | Decrement Int -- -
                      | BPrint        -- .
                      | BRead         -- ,
                      | LoopL         -- [
                      | LoopR         -- ]
                      | SetZero       -- Optimization for [-]
                      | MulR Int Int  -- Optimization for [->+<]
                      | MulL Int Int  -- Optimization for [-<+>]
                      | End           -- Just signalize about end
                      deriving (Show, Eq)

data Tape a = Tape {left, right :: [a], value :: !a} deriving (Show)

emptyTape8 :: Tape Word8
emptyTape8 = Tape{left = zeros, value = 0, right = zeros}
  where zeros = 0 : zeros

emptyTape16 :: Tape Word16
emptyTape16 = Tape{left = zeros, value = 0, right = zeros}
  where zeros = 0 : zeros
