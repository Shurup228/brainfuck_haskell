module Main (main) where

import Foundation
import Foundation.IO
import Foundation.String
import Foundation.VFS.FilePath
import BrainRun

parseArgs :: [String] -> IO (Maybe (Bool, [String]))
parseArgs args
  | length args == 2 &&  "-O" `elem` args  =
      (putStrLn "Using 8 bit cells and optimizations") >>
      (return $ Just (True, filter (/= "-O") . filter (/= "8") $ args))
  | length args == 1                      =
      (putStrLn "Using default configutarion") >>
      (return $ Just (False, args))
  | otherwise                             =
      (putStrLn $ "Wrong args\nUsage:\n\t[-O]     use optimizations(can be" <>
      "omitted)\n\t[name]    of the prog to run\n ") >>
      (return Nothing)

run :: (Bool, [String]) -> IO ()
run (o, name) = (readFile . maybe "" (fromString . toList . head) $ nonEmpty name) >>=
                (let run' = if o then runOpt8 else run8 in run' . fromBytesUnsafe)

main :: IO ()
main = getArgs >>= parseArgs >>= maybe mempty run
