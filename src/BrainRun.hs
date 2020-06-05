module BrainRun (run8, runOpt8) where

import Foundation
import Foundation.IO
import BrainData
import BrainOpt

toTape :: Code -> Tape BrainfuckCommand
toTape (x:xs) = Tape [] xs x
toTape _ = Tape [] [] End

checkSyntax :: String -> String
checkSyntax xs = if isNothing p then xs else mempty
  where 
    p :: Maybe String
    go :: Int -> [Char] -> Maybe String
    p = go 0 . toList $ filter (`elem` ("[]" :: String)) xs
    go 0    []      = Nothing
    go (-1) _       = Just xs
    go acc ('[':ys) = go (acc+1) ys
    go acc (']':ys) = go (acc-1) ys
    go _    _       = Just xs

parseBrainfuck :: String -> Code
parseBrainfuck code = (mapMaybe charToBf $ toList code) <> [End]
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
                   _   -> Nothing

moveL, moveR :: Word8 -> Tape a -> Tape a
moveL i t = go i t where
  go :: Word8 -> Tape a -> Tape a
  go 0 t = t
  go i (Tape (l:ls) rss v) = go (i - 1) (Tape ls (v:rss) l)
  go _ t = t
moveR i t = go i t where
  go :: Word8 -> Tape a -> Tape a
  go 0 t = t
  go i (Tape lss (r:rs) v) = go (i - 1) (Tape (v:lss) rs r)
  go _ t = t

toStr :: Word8 -> String
toStr a = fromList [toEnum $ fromEnum a]

exec :: Tape Word8 -> [BrainfuckCommand] -> IO ()
exec tape = go tape . toTape
  where
    go :: Tape Word8 -> Tape BrainfuckCommand -> IO ()
    go _                 (Tape _ _ End)                = return (mempty)
    go tape@(Tape _ _ v) code@(Tape _ _ (Increment h)) =
      go tape{value = v + fromIntegral h} $ moveR 1 code
    go tape@(Tape _ _ v) code@(Tape _ _ (Decrement h)) =
      go tape{value = v - fromIntegral h} $ moveR 1 code
    go tape@(Tape _ _ v) code@(Tape _ _ SetZero)       =
      go tape{value = 0} $ moveR 1 code
    go tape@(Tape _ _ v) code@(Tape _ _ BPrint)        =
      (putStr $ toStr v) >> (go tape (moveR 1 code))
    go tape              code@(Tape _ _ (GoRight h))   =
      go (moveR (fromIntegral h) tape) $ moveR 1 code
    go tape              code@(Tape _ _ (GoLeft h))    =
      go (moveL (fromIntegral h) tape) $ moveR 1 code
    go tape              code@(Tape _ _ BRead)         =
      hGet stdin 1 >>=
      (\ch -> go tape{value = maybe 0 (fromIntegral . head) $ nonEmpty ch} $
              moveR 1 code)

    -- ========================= Optimizations ========================

    go tape@(Tape _ (_:rs) v) code@(Tape _ _ (MulR h x)) =
      let new_f = moveR (fromIntegral h) tape{value = 0}
          v2    = value new_f in
        go (moveL (fromIntegral h) $ new_f{value = (fromIntegral x) * v + v2}) $
        moveR 1 code
    go tape@(Tape _ (_:rs) v) code@(Tape _ _ (MulL h x)) =
      let new_f = moveL (fromIntegral h) tape{value = 0}
          v2    = value new_f in
        go (moveR (fromIntegral h) $ new_f{value = (fromIntegral x) * v + v2}) $
        moveR 1 code

    go tape@(Tape _ _ v) code@(Tape _ _ LoopL)
      | v == 0    = go tape $ seekLoopR 0 $ moveR 1 code
      | otherwise = go tape $ moveR 1 code
    go tape@(Tape _ _ v) code@(Tape _ _ LoopR)
      | v /= 0    = go tape $ seekLoopL 0 $ moveL 1 code
      | otherwise = go tape $ moveR 1 code

    -- For moving scope from [ to ]
    seekLoopR, seekLoopL :: Int -> Tape BrainfuckCommand -> Tape BrainfuckCommand
    seekLoopR 0 code@(Tape _ _ LoopR) = code
    seekLoopR p code@(Tape _ _ LoopL) = seekLoopR (succ p) $ moveR 1 code
    seekLoopR p code@(Tape _ _ LoopR) = seekLoopR (pred p) $ moveR 1 code
    seekLoopR p code                  = seekLoopR p $ moveR 1 code

    seekLoopL 0 code@(Tape _ _ LoopL) = code
    seekLoopL p code@(Tape _ _ LoopR) = seekLoopL (succ p) $ moveL 1 code
    seekLoopL p code@(Tape _ _ LoopL) = seekLoopL (pred p) $ moveL 1 code
    seekLoopL p code                  = seekLoopL p $ moveL 1 code

run8 :: String -> IO ()
run8 = exec emptyTape8 . optimize . parseBrainfuck . checkSyntax

runOpt8 :: String -> IO ()
runOpt8  = exec emptyTape8 . optimize . optimize . parseBrainfuck . checkSyntax
