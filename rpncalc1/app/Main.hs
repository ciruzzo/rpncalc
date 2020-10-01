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
      | length acc /= 1 = error $ "syntax error" ++ show acc
      | otherwise       = head acc

    loop acc (o:rst)
      | o == "+"       = loop ((p2+p1):ps) rst
      | o == "-"       = loop ((p2-p1):ps) rst
      | o == "*"       = loop ((p2*p1):ps) rst
      | o == "/"       = loop ((p2/p1):ps) rst
      | otherwise      = loop ((read o :: Double):acc) rst 
        where 
          (p1:p2:ps) = acc


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
