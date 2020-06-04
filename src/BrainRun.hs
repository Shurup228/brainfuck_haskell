{-# LANGUAGE NoImplicitPrelude #-}

module BrainRun (run8, runOpt8, run16, runOpt16) where

import Protolude
import qualified Data.Text as T
import BrainData
import BrainOpt

toTape :: Code -> Tape BrainfuckCommand
toTape (x:xs) = Tape [] xs x

checkSyntax :: [Char] -> [Char]
checkSyntax xs = if isNothing p then xs else ""
  where 
    p = go 0 $ filter (`elem` "[]") xs
    go 0    []      = Nothing
    go (-1) _       = Just xs
    go acc ('[':ys) = go (acc+1) ys
    go acc (']':ys) = go (acc-1) ys
    go _    _       = Just xs

parseBrainfuck :: [Char] -> Code
parseBrainfuck code = mapMaybe charToBf code ++ [End]
  where
    charToBf x = case x of
                   '>' -> Just $ GoRight 1
                   '<' -> Just $ GoLeft 1
                   '+' -> Just $ Increment 1
                   '-' -> Just $ Decrement 1
                   '.' -> Just BPrint
                   ',' -> Just BRead
                   '[' -> Just LoopL
                   ']' -> Just LoopR
                   c   -> Nothing

moveL, moveR :: Tape a -> Tape a
moveL (Tape (l:ls) rss v) = Tape ls (v:rss) l
moveR (Tape lss (r:rs) v) = Tape (v:lss) rs r

exec tape = go tape . toTape
  where
    go _                 (Tape _ _ End)                = return ()
    go tape@(Tape _ _ v) code@(Tape _ _ (Increment h)) = go tape{value = v+fromIntegral h} $ moveR code
    go tape@(Tape _ _ v) code@(Tape _ _ (Decrement h)) = go tape{value = v-fromIntegral h} $ moveR code
    go tape@(Tape _ _ v) code@(Tape _ _ SetZero)       = go tape{value = 0} $ moveR code
    go tape@(Tape _ _ v) code@(Tape _ _ BPrint)        = (putStr . T.singleton . chr $ fromIntegral v) >> go tape (moveR code)
    go tape              code@(Tape _ _ (GoRight h))   = go (applyN h moveR tape) $ moveR code
    go tape              code@(Tape _ _ (GoLeft h))    = go (applyN h moveL tape) $ moveR code
    go tape              code@(Tape _ _ BRead)         = getLine >>= (\char -> go tape{value = fromIntegral . ord. T.head $ char} $ moveR code)

    -- ========================= Optimizations ========================

    go tape@(Tape _ (_:rs) v) code@(Tape _ _ (MulR h x)) = let new_f = applyN h moveR tape{value = 0} 
                                                               v2    = value new_f in
                                                               go (applyN h moveL $ new_f{value = (fromIntegral x)*v+v2}) $ moveR code
    go tape@(Tape _ (_:rs) v) code@(Tape _ _ (MulL h x)) = let new_f = applyN h moveL tape{value = 0} 
                                                               v2    = value new_f in
                                                               go (applyN h moveR $ new_f{value = (fromIntegral x)*v+v2}) $ moveR code

    go tape@(Tape _ _ v) code@(Tape _ _ LoopL)
      | v == 0    = go tape $ seekLoopR 0 $ moveR code
      | otherwise = go tape $ moveR code
    go tape@(Tape _ _ v) code@(Tape _ _ LoopR)
      | v /= 0    = go tape $ seekLoopL 0 $ moveL code
      | otherwise = go tape $ moveR code

    -- For moving scope from [ to ]
    seekLoopR, seekLoopL :: Int -> Tape BrainfuckCommand -> Tape BrainfuckCommand
    seekLoopR 0 code@(Tape _ _ LoopR) = code
    seekLoopR p code@(Tape _ _ LoopL) = seekLoopR (succ p) $ moveR code
    seekLoopR p code@(Tape _ _ LoopR) = seekLoopR (pred p) $ moveR code
    seekLoopR p code                  = seekLoopR p $ moveR code

    seekLoopL 0 code@(Tape _ _ LoopL) = code
    seekLoopL p code@(Tape _ _ LoopR) = seekLoopL (succ p) $ moveL code
    seekLoopL p code@(Tape _ _ LoopL) = seekLoopL (pred p) $ moveL code
    seekLoopL p code                  = seekLoopL p $ moveL code

run8     = exec emptyTape8 . optimize . parseBrainfuck . checkSyntax
runOpt8  = exec emptyTape8 . optimize . optimize . parseBrainfuck . checkSyntax
run16    = exec emptyTape16 . optimize . parseBrainfuck . checkSyntax
runOpt16 = exec emptyTape16 . optimize . optimize . parseBrainfuck . checkSyntax
