module Expr where
import Control.Applicative ((<|>))
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
          | StrLit String
          | ReadStr
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

nameToValue:: [(Name, Value)] -> Name -> Maybe Value
nameToValue vars name = foldr (\(x, val) acc -> if x == name then Just val else acc) Nothing vars

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
eval vars (StrLit s) = Just (StrVal s)

evalIO :: [(Name, Value)] -> Expr -> IO (Maybe Value)
evalIO vars expr = case expr of
    ReadStr -> do
        input <- getLine  -- Read input from the user
        return $ Just (StrVal input)  -- Wrap the input as a StrVal
    _ -> return $ eval vars expr  -- For all other expressions, defer to the existing eval function


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

pStringLit :: Parser Expr
pStringLit = do
    char '\"'
    content <- many (sat (/= '\"'))  -- Parse until the closing quote, excluding the quote character.
    char '\"'
    return $ StrLit content

pCommand :: Parser Command
pCommand = do varName <- identifier  -- Allows for multi-character variable names.
              space  -- Optional whitespace before '='
              char '='
              space  -- Optional whitespace after '='
              expr <- pExpr
              return (Set varName expr)
            ||| do string "print"
                   space
                   expr <- pExpr
                   return (Print expr)

pReadStr :: Parser Expr
pReadStr = do
    string "ReadStr"  -- Adjust the string to match the constructor name
    return ReadStr

pExpr :: Parser Expr
pExpr = pReadStr
    <|> do t <- pTerm
           (do space
               op <- char '+' <|> char '-'
               space
               e <- pExpr
               case op of
                 '+' -> return (Add t e)
                 '-' -> return (Sub t e)
               ) <|> return t

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
           (do space
               op <- char '*' <|> char '/'
               space
               t <- pTerm
               case op of
                 '*' -> return (Mul f t)
                 '/' -> return (Div f t)
               ) <|> return f