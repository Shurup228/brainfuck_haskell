{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import BrainRun
import Protolude
import Data.List (delete)

readName = readFile . headDef ""


parseArgs args
  | "8" `elem` args && "-O2" `elem` args  = putStrLn "Using 8 bit cells and optimizations" >> (readName . delete "-O2" . delete "8" $ args) >>= runOpt8 . toS
  | "16" `elem` args && "-O2" `elem` args = putStrLn "Using 16 bit cells and optimizations" >> (readName . delete "-O2" . delete "16" $ args) >>= runOpt16 . toS
  | "8" `elem` args                       = putStrLn "Using 8 bit cells without optimizations" >> (readName . delete "8" $ args) >>= run8 . toS
  | "16" `elem` args                      = putStrLn "Using 16 bit cells without optimizations " >> (readName . delete "16" $ args) >>= run16 . toS
  | length args == 1                      = putStrLn "Using default configutarion" >> readName args >>= runOpt8 . toS
  | otherwise                             = putStrLn "Wrong args\nUsage:\n\t[8 or 16] use Word8 or Word16\n\t[-O2]     use optimizations(can be omitted)\n\t[name]    of the prog to run\nDefault configuration is 8 bit cells with optimizations"


main = getArgs >>= parseArgs
