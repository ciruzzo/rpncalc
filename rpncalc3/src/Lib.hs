{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( rpncalc
     ,rev
     ,Exp (..)
     ,Ops (..)
     ,pExp
     ,showParseResult
    ) where

import Data.Text (pack, unpack, splitOn, Text, split, intercalate)
import Data.Text.Encoding
import Text.Read (readMaybe)

import Data.Attoparsec.Text
import Control.Applicative
import Data.List (reverse)

-- 'reversed' RPN expression
data Ops = Plus | Minus | Multiply | Divide deriving (Eq,Show)
data Exp = Val Double | Expr Ops Exp Exp  deriving (Eq,Show)

pOps :: Parser Ops
pOps = (string "+ " >> return Plus) <|> 
       (string "- " >> return Minus) <|>
       (string "* " >> return Multiply) <|>
       (string "/ " >> return Divide) 

pVal :: Parser Exp
pVal = Val <$> double <* space

pExp :: Parser Exp
pExp = pVal <|> 
       Expr <$> pOps <*> pExp <*> pExp

rev :: String -> Text
rev s = intercalate " " $ Data.List.reverse $ split (==' ') $ pack s

-- ex2 op ex1, because ex1,ex2 are reversed
calcExp :: Exp -> Double
calcExp (Val x) = x
calcExp (Expr op ex1 ex2) 
  | op == Plus  = (calcExp ex2) + (calcExp ex1)
  | op == Minus = (calcExp ex2) - (calcExp ex1)
  | op == Multiply = (calcExp ex2) * (calcExp ex1)
  | op == Divide   = (calcExp ex2) / (calcExp ex1)

showParseResult :: Show a => Result a -> Either String a
showParseResult (Done _ r) = Right r
showParseResult r          = Left $ show r

rpncalc :: String -> Double
rpncalc input = do
                let res = showParseResult $ parse pExp (rev $ " "++input) `feed` ""
                case res of 
                  Right r -> calcExp $ r 
                  Left  r -> error $ "error: parse failed " ++ r

