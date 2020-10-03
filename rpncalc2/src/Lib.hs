module Lib
    ( rpncalc
    ) where

import Data.Text (pack, unpack, splitOn)
import Data.Text.Encoding
import Text.Read (readMaybe)
import Control.Monad.State

type CalcState = [Double]

docalc :: String -> State CalcState ()
docalc op = do
  stack <- get
  case (length stack <2) of 
    True  -> error $ "syntax error" ++ show stack ++ show op
    _     -> let (p2:p1:ps) = stack
              in case op of 
                "+" -> put((p1+p2):ps)
                "-" -> put((p1-p2):ps)
                "*" -> put((p1*p2):ps)
                "/" -> put((p1/p2):ps)

process :: [String] -> State CalcState Double
process [] = do
  stack <- get
  case (length stack /= 1) of 
    True -> error $ "syntax error" ++ show stack
    _    -> return $ head stack
process (x:xs) = do
  case (readMaybe x :: Maybe Double) of
    (Just n) -> do 
                  stack <- get 
                  put (n:stack) 
    _        -> docalc x
  process xs

initState = []
rpncalc :: String -> Double
rpncalc input = evalState (process (map unpack $ splitOn (pack " ") (pack input))) initState


