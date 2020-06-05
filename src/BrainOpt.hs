module BrainOpt (optimize) where

import Foundation
import BrainData

optimize :: Code -> Code

-- ================================= Block of high lvl optimizations ====================================
optimize (LoopL:Decrement 1:GoRight h1:Increment x:GoLeft h2:LoopR:xs)
  | h1 == h2  = MulR h1 x: optimize xs
  | otherwise = LoopL:Decrement 1: GoRight h1: Increment 1: GoLeft h2: LoopR: optimize xs

optimize (LoopL:Decrement 1:GoLeft h1:Increment x:GoRight h2:LoopR:xs)
  | h1 == h2  = MulL h1 x: optimize xs
  | otherwise = LoopL:Decrement 1: GoLeft h1: Increment 1: GoRight h2: LoopR: optimize xs
-- For copy loops e.g. [->+<]
-- ======================================== End of high level ===========================================

-- ================================== Block of low level optimizations ==================================

optimize (LoopL:Decrement 1:LoopR:xs)     = SetZero: optimize xs
-- [-] or clear cell optimization
optimize code@(Increment 1:Increment 1:_) =
  Increment (fromCount . length $ takeWhile (== Increment 1) code):
  (optimize $ dropWhile (== Increment 1) code)
-- For multiple + in a row
optimize code@(Decrement 1:Decrement 1:_) =
  Decrement (fromCount . length $ takeWhile (== Decrement 1) code):
  (optimize $ dropWhile (== Decrement 1) code)
-- Same for -
optimize code@(GoRight 1:GoRight 1:_)     =
  GoRight (fromCount . length $ takeWhile (== GoRight 1) code):
  (optimize $ dropWhile (== GoRight 1) code)
-- Same for >
optimize code@(GoLeft 1:GoLeft 1:_)       =
  GoLeft (fromCount . length $ takeWhile (== GoLeft 1) code):
  (optimize $ dropWhile (== GoLeft 1) code)
-- Same for <
optimize [End]                            = [End]
optimize xs = xs -- Skip if no optimizations can be applied

-- ======================================== End of low level ============================================
