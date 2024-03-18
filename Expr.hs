module Expr where

import Parsing

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
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

-- maybeAdd :: Maybe Value -> Maybe Value -> Maybe Value
-- maybeAdd Nothing _ = error "Invalid Variable Name"
-- maybeAdd _ Nothing = error "Invalid Variable Name"
-- maybeAdd (Just (IntVal x)) (Just (IntVal y)) = Just (IntVal (x + y))

maybeValueToInt :: Maybe Value -> Maybe Int
maybeValueToInt (Just (IntVal x)) = Just x
maybeValueToInt _ = Nothing

nameToValue:: [(Name, Int)] -> Name -> Maybe Value
nameToValue vars name = foldr(\x acc -> if fst x == name then Just (IntVal (snd x)) else acc) Nothing vars

eval :: [(Name, Value)] -> Expr -> Maybe Value
eval vars (VarName x) = lookup x vars  -- Adjusted for simplicity
eval vars (Val x) = Just (IntVal x)
eval vars (Add x y) = applyOp (+) (eval vars x) (eval vars y)
eval vars (Sub x y) = applyOp (-) (eval vars x) (eval vars y)
eval vars (Mul x y) = applyOp (*) (eval vars x) (eval vars y)
eval vars (Div x y) = applyDiv (eval vars x) (eval vars y)
eval vars (ToString x) = case eval vars x of
                            Just (IntVal n) -> Just (StrVal (show n))
                            _ -> Nothing

applyOp :: (Int -> Int -> Int) -> Maybe Value -> Maybe Value -> Maybe Value
applyOp op (Just (IntVal a)) (Just (IntVal b)) = Just (IntVal (op a b))
applyOp _ _ _ = Nothing

applyDiv :: Maybe Value -> Maybe Value -> Maybe Value
applyDiv (Just (IntVal a)) (Just (IntVal b))
    | b /= 0    = Just (IntVal (a `div` b))
    | otherwise = Nothing  -- Handle division by zero as an error or return Nothing
applyDiv _ _ = Nothing

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
              return (Add t e)            --Adding a number to an expression
            ||| do char '-'
                   e <- pExpr
                   return (Sub t e)       --Subtracting a term from an expression
                 ||| return t

pFactor :: Parser Expr
pFactor = do n <- natural
             return (Val n)  -- Multi-digit Number
           ||| do t <- identifier
                  return (VarName t)  -- Variable Name
           ||| do char '('
                  e <- pExpr
                  char ')'
                  return e  -- Expression in parentheses

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pFactor
              return(Mul t f)             --Multiplying two factors
            ||| do char '/'
                   t <- pFactor
                   return(Div t f)        --Dividing two factors
                 ||| return f
