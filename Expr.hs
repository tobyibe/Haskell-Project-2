module Expr where

import Parsing

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | ToString Expr
          | Val Int
          | VarName Name
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

data Value = IntVal Int | StrVal String
  deriving Show

maybeAdd :: Maybe Value -> Maybe Value -> Maybe Value
maybeAdd Nothing _ = error "Invalid Variable Name"
maybeAdd _ Nothing = error "Invalid Variable Name"
maybeAdd (Just (IntVal x)) (Just (IntVal y)) = Just (IntVal (x + y))

maybeValueToInt :: Maybe Value -> Int
maybeValueToInt (Just (IntVal x)) = x

nameToValue:: [(Name, Int)] -> Name -> Maybe Value
nameToValue vars name = foldr(\x acc -> if fst x == name then Just (IntVal (snd x)) else acc) Nothing vars

eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)

eval vars (VarName x) = nameToValue vars x -- for StrVal values, string is variable name, replace it with value 
eval vars (Val x) = Just (IntVal x) -- For IntVal values, just give the value directly
eval vars (Add x y) = maybeAdd (eval vars x) (eval vars y)
eval vars (ToString x) = Just (StrVal (show (maybeValueToInt (eval vars x))))

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- letter
              char '='
              e <- pExpr
              return (Set [t] e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   error "Subtraction not yet implemented!" 
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (digitToInt d))
           ||| do v <- letter
                  error "Variables not yet implemented" 
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              error "Multiplication not yet implemented" 
            ||| do char '/'
                   t <- pTerm
                   error "Division not yet implemented" 
                 ||| return f
