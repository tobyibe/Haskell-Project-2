module Expr where
import Control.Applicative ((<|>))
import Parsing

type Name = String

-- The Expr data type represents expressions in our language. It includes various operations like addition, subtraction, multiplication, division, absolute value, modulus, power, string conversion, integer values, variable names, string literals, string reading, string concatenation, and integer conversion.
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr --Absolute val
          | Mod Expr Expr --Modulus
          | Pow Expr Expr --Exponent
          | ToString Expr
          | Val Int
          | VarName Name
          | StrLit String
          | ReadStr
          | Concat Expr Expr
          | ToInt Expr
  deriving Show

-- The Command data type represents commands in our REPL. It includes setting a variable to an expression and printing the result of an expression.
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

-- The Value data type represents values that expressions can evaluate to. It includes integer values and string values.
data Value = IntVal Int | StrVal String
  deriving Show

-- maybeAdd :: Maybe Value -> Maybe Value -> Maybe Value
-- maybeAdd Nothing _ = error "Invalid Variable Name"
-- maybeAdd _ Nothing = error "Invalid Variable Name"
-- maybeAdd (Just (IntVal x)) (Just (IntVal y)) = Just (IntVal (x + y))

-- The maybeValueToInt function converts a Maybe Value to a Maybe Int. It's used when we need to perform an operation on an integer value, but we have a Maybe Value.
maybeValueToInt :: Maybe Value -> Maybe Int
maybeValueToInt (Just (IntVal x)) = Just x
maybeValueToInt _ = Nothing

-- The nameToValue function takes a list of variable bindings and a variable name, and returns the value of the variable if it exists.
nameToValue:: [(Name, Value)] -> Name -> Maybe Value
nameToValue vars name = foldr (\(x, val) acc -> if x == name then Just val else acc) Nothing vars

-- The eval function takes a list of variable bindings and an expression, and evaluates the expression. It handles different types of expressions like variable names, integer values, addition, subtraction, multiplication, division, modulus, power, string conversion, string literals, string concatenation, and integer conversion.
eval :: [(Name, Value)] -> Expr -> Maybe Value
eval vars (VarName x) = lookup x vars  -- Adjusted for simplicity
eval vars (Val x) = Just (IntVal x)
eval vars (Add x y) = applyOp (+) (eval vars x) (eval vars y)  -- Add two expressions
eval vars (Sub x y) = applyOp (-) (eval vars x) (eval vars y)  -- Subtract two expressions
eval vars (Mul x y) = applyOp (*) (eval vars x) (eval vars y)  -- Multiply two expressions
eval vars (Div x y) = applyDiv (eval vars x) (eval vars y)  -- Divide two expressions
eval vars (Mod x y) = applyOp mod (eval vars x) (eval vars y)  -- Modulus of two expressions
eval vars (Pow x y) = applyOp (^) (eval vars x) (eval vars y)  -- Power of two expressions
eval vars (ToString x) = case eval vars x of
                            Just (IntVal n) -> Just (StrVal (show n))
                            _ -> Nothing
eval vars (StrLit s) = Just (StrVal s)  -- A string literal evaluates to itself
eval vars (Concat x y) = case (eval vars x, eval vars y) of
                            (Just (StrVal a), Just (StrVal b)) -> Just (StrVal (a ++ b))
                            _ -> Nothing  -- Handle errors or incompatible types
eval vars (ToInt e) = case eval vars e of  -- Convert an expression to an integer
                          Just (StrVal s) -> case reads s :: [(Int, String)] of
                                               [(n, "")] -> Just (IntVal n)  -- Successfully parsed the entire string
                                               _ -> Nothing  -- Parsing failed or didn't consume the whole string
                          _ -> Nothing

-- The evalIO function is similar to eval, but it can perform IO operations. It's used for expressions that need to read input from the user.
evalIO :: [(Name, Value)] -> Expr -> IO (Maybe Value)
evalIO vars expr = case expr of
    ReadStr -> do
        input <- getLine  -- Read input from the user
        return $ Just (StrVal input)  -- Wrap the input as a StrVal
    _ -> return $ eval vars expr  -- For all other expressions, defer to the existing eval function


-- The applyOp function takes an operation and two Maybe Values, and applies the operation if both Values are Just IntVals. Otherwise, it returns Nothing.
applyOp :: (Int -> Int -> Int) -> Maybe Value -> Maybe Value -> Maybe Value
applyOp op (Just (IntVal a)) (Just (IntVal b)) = Just (IntVal (op a b))
applyOp _ _ _ = Nothing

-- The applyDiv function is similar to applyOp, but it's specifically for division. It checks if the divisor is zero and returns Nothing in that case to avoid a division by zero error.
applyDiv :: Maybe Value -> Maybe Value -> Maybe Value
applyDiv (Just (IntVal a)) (Just (IntVal b))
    | b /= 0    = Just (IntVal (a `div` b))
    | otherwise = Nothing  -- Handle division by zero as an error or return Nothing
applyDiv _ _ = Nothing

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- The `pStringLit` parser function parses string literals enclosed in double quotes.
-- It reads characters until it encounters another double quote, excluding the quote character itself,
-- and constructs a `StrLit` expression containing the parsed string.
pStringLit :: Parser Expr
pStringLit = do
    char '\"'
    content <- many (sat (/= '\"'))  -- Parse until the closing quote, excluding the quote character.
    char '\"'
    return $ StrLit content

-- The `pCommand` parser function attempts to parse REPL commands.
-- It supports two commands: setting a variable to the result of an expression (`Set`),
-- and printing the result of an expression (`Print`). This function uses the `pExpr`
-- parser to parse the expressions involved in these commands.
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

-- The `pReadStr` parser function recognizes the "ReadStr" command used to read a string from the user input.
-- It matches the literal string "ReadStr" and constructs a `ReadStr` expression.
pReadStr :: Parser Expr
pReadStr = do
    string "ReadStr"  -- Adjust the string to match the constructor name
    return ReadStr

-- The `pConcat` parser function parses string concatenation expressions using the "++" operator.
-- It parses two expressions and constructs a `Concat` expression combining them.
pConcat :: Parser Expr
pConcat = do
    leftExpr <- pTerm  -- Parse the left-hand operand
    space
    _ <- string "++"  -- Match the concatenation operator
    space
    rightExpr <- pTerm  -- Parse the right-hand operand
    return $ Concat leftExpr rightExpr  -- Return a Concat expression


-- The `pExpr` parser function composes various expression parsers, attempting to parse different kinds of expressions.
-- It tries to parse string concatenation, string reading, string literals, and other expressions defined by `pTerm`.
pExpr :: Parser Expr
pExpr = pConcat  
    <|> pReadStr
    <|> pStringLit
    <|> do t <- pTerm
           (do space
               op <- char '*' <|> char '/' <|> char '+' <|> char '-' <|> char '^' <|> char '%'
               space
               e <- pExpr
               case op of
                 '*' -> return (Mul t e)
                 '/' -> return (Div t e)
                 '+' -> return (Add t e)
                 '-' -> return (Sub t e)
                 '^' -> return (Pow t e)
                 '%' -> return (Mod t e)
               ) <|> return t
  
-- The `pNegativeExpr` parser function handles negative expressions.
-- It specifically looks for a "-" sign followed by an expression, and constructs a multiplication
-- by -1 expression to represent the negative value.  
pNegativeExpr :: Parser Expr
pNegativeExpr = do
    space
    char '-'
    space
    e <- pExpr
    return(Mul (Val (-1)) e)

-- The `pFactor` parser function parses factors in expressions, such as numbers, variable names, and expressions enclosed in parentheses.
-- It's used as part of parsing larger arithmetic expressions.
pFactor :: Parser Expr
pFactor = do n <- natural
             return (Val n)  -- Multi-digit Number
           ||| do t <- identifier
                  return (VarName t)  -- Variable Name
           ||| do char '('
                  e <- pExpr
                  char ')'
                  return e  -- Expression in parentheses

-- The `pTerm` parser function composes the factor parsing with handling for arithmetic operations,
-- allowing for expressions that involve multiplication, division, and other operations defined in `pFactor`.
pTerm :: Parser Expr
pTerm = pNegativeExpr <|> pFactor <|> do
    f <- pFactor
    (do space
        op <- char '*' <|> char '/' <|> char '+' <|> char '-' <|> char '^' <|> char '%'
        space
        t <- pTerm
        case op of
          '*' -> return (Mul f t)
          '/' -> return (Div f t)
          '+' -> return (Add f t)
          '-' -> return (Sub f t)
          '^' -> return (Pow f t)
          '%' -> return (Mod f t)
        ) <|> return f