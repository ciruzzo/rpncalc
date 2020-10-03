{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, unpack, splitOn, Text, split, intercalate)
import Data.Attoparsec.Text
import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT $ "Test1" ~: (unpack $ rev "1 2 +") ~?= "+ 2 1"
  runTestTT $ "Test2" ~: (showParseResult $ parse pExp (pack "+ 2 1 ") `feed` "") ~?= Right (Expr Plus (Val 2.0) (Val 1.0))
  runTestTT $ "Test3" ~: rpncalc "1 2 +" ~?= 3
  runTestTT $ "Test4" ~: rpncalc "2.5 1.3 - 3.7 4.3 + *" ~?= 9.6
  return ()
