import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT $ "Test1" ~: rpncalc "1 2 +" ~?= 3
  runTestTT $ "Test2" ~: rpncalc "2.5 1.3 - 3.7 4.3 + *" ~?= 9.6
  return ()
