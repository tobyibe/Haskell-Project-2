module REPL where

import Expr
import Parsing
import Data.Char (isSpace)

data LState = LState { vars :: [(Name, Value)] }   -- updated to Value instead of int to accept strings and integers

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val [] = [(name, val)]
updateVars name val xs = filteredList ++ [(name, val)] where filteredList = filter (\(n, _) -> n /= name) xs

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name xs = filter (\x -> fst x /= name) xs

process :: LState -> Command -> IO ()
process st (Set var e) = do
    case eval (vars st) e of
        Just val -> do
            let newVars = updateVars var val (vars st)
            let st' = LState {vars = newVars}
            repl st'
        Nothing -> putStrLn "Error evaluating expression." >> repl st
process st (Print e) = do
    case eval (vars st) e of
        Just val -> putStrLn (show val)
        Nothing -> putStrLn "Error evaluating expression." >> repl st

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
   
-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do
    putStr "> "
    inp <- getLine
    let trimmedInp = trim inp
    if trimmedInp == "quit"  -- Use the trimmed input for comparison
    then putStrLn "Exiting..."
    else case parse pCommand trimmedInp of  -- Also use trimmed input for parsing
            [(cmd, "")] -> process st cmd
            _ -> do putStrLn "Parse error"
                    repl st

