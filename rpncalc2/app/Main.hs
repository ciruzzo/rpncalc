module Main where

import System.IO 
import System.Console.Haskeline

import Data.Text (pack, unpack, splitOn)
import Data.Text.Encoding
import Text.Read (readMaybe)

import Control.Monad.State

type CalcState = [Double]

docalc :: String -> State CalcState ()
docalc op = do
  stack <- get
  let l = length stack 
    in case (l<2) of 
            True  -> error $ "syntax error" ++ show stack
            _     -> let (p2:p1:ps) = stack
                       in case op of 
                           "+" -> put((p1+p2):ps)
                           "-" -> put((p1-p2):ps)
                           "*" -> put((p1*p2):ps)
                           "/" -> put((p1/p2):ps)

process :: [String] -> State CalcState Double
process [] = do
  stack <- get
  return $ head stack
process (x:xs) = do
  let opn = (readMaybe x :: Maybe Double)
    in case opn of 
                (Just n) -> do {stack <- get; put (n:stack) }
                _        -> (docalc x)
  process xs

initState = []

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "RPN calc> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do outputStrLn $ show $ evalState (process (map unpack $ splitOn (pack " ") (pack input))) initState
      loop
