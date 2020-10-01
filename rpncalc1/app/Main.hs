module Main where

import System.IO 
import System.Console.Haskeline

import Data.Text (pack, unpack, splitOn)
import Data.Text.Encoding

rpncalc :: String -> Double
rpncalc str = loop [] $ map unpack $ splitOn (pack " ") (pack str)
  where 
    loop :: [Double] -> [String] -> Double
    loop acc [] 
      | length acc > 1 = error $ "syntax error" ++ show acc
      | otherwise      = head acc

    loop acc (o: rst) = case o of 
      "+" -> loop (acc'++[p1+p2]) rst
      "-" -> loop (acc'++[p1-p2]) rst
      "*" -> loop (acc'++[p1*p2]) rst
      "/" -> loop (acc'++[p1/p2]) rst
      _   -> loop (acc ++ [read o :: Double]) rst 

      where 
        t = reverse acc
        [p1,p2] = reverse $ take 2 t
        acc'    = reverse $ drop 2 t

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
