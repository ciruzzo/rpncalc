module Lib
    ( rpncalc
    ) where

import Data.Text (pack, unpack, splitOn)
import Data.Text.Encoding
import Text.Read (readMaybe)

rpncalc :: String -> Double
rpncalc str = loop [] $ map unpack $ splitOn (pack " ") (pack str)
  where 
    loop :: [Double] -> [String] -> Double
    loop acc [] 
      | length acc /= 1 = error $ "syntax error" ++ show acc
      | otherwise       = head acc
    loop acc (x:xs) = do
      case (readMaybe x :: Maybe Double) of
        (Just n) -> loop (n:acc) xs
        _        -> loop (docalc acc x) xs 

docalc :: [Double] -> String -> [Double]
docalc (p2:p1:ps) op
  | op == "+" = (p1+p2):ps
  | op == "-" = (p1-p2):ps
  | op == "*" = (p1*p2):ps
  | op == "/" = (p1/p2):ps

