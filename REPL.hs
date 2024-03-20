module REPL where

import Expr
import Parsing

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


-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do
    putStr "> "
    inp <- getLine
    if inp == "quit"  -- Check if the user input is "quit"
    then putStrLn "Exiting..."  -- If so, print an exit message and terminate
    else case parse pCommand inp of  -- Otherwise, proceed with parsing and processing the command
            [(cmd, "")] -> process st cmd  -- If parsing is successful, process the command
            _ -> do putStrLn "Parse error"  -- If parsing fails, print an error message
                    repl st  -- And call `repl` recursively to continue the loop
