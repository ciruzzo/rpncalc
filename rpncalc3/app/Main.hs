module Main (main) where

import System.IO 
import System.Console.Haskeline
import Lib

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "RPN calc> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do outputStrLn $ show $ rpncalc input
      loop
