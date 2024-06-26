module REPL where

import Expr
import Parsing
import Data.Char (isSpace)
import System.IO (hFlush, stdout)

-- | Represents the current state of the Read-Eval-Print Loop (REPL),
-- including variable bindings. It is designed to be extensible for future features.
data LState = LState { vars :: [(Name, Value)] }

-- | Initializes the REPL state with no variables.
initLState :: LState
initLState = LState []

-- | Updates the variable bindings in the current state. If the variable already exists,
-- its value is updated; otherwise, a new variable binding is added.
-- @param name The name of the variable to update or add.
-- @param val The value to assign to the variable.
-- @param xs The current list of variable bindings.
-- @return An updated list of variable bindings.
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val [] = [(name, val)]
updateVars name val xs = filteredList ++ [(name, val)]
  where filteredList = filter (\(n, _) -> n /= name) xs  -- Remove existing binding if present.

-- | Removes a variable binding from the current state.
-- @param name The name of the variable to remove.
-- @param xs The current list of variable bindings.
-- @return An updated list of variable bindings without the specified variable.
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name xs = filter (\x -> fst x /= name) xs

-- | Processes a command input by the user in the REPL.
-- This could be setting a variable or printing an expression.
-- @param st The current state of the REPL.
-- @param cmd The command to process.
process :: LState -> Command -> IO ()
process st (Set var e) = do
    case e of
        ReadStr -> do
            input <- getLine -- Directly read input from the user
            let newVars = updateVars var (StrVal input) (vars st) -- Create a StrVal from input and update vars
            let st' = LState {vars = newVars}
            repl st' -- Continue with the updated state
        _ -> do
            val <- case eval (vars st) e of
                Just value -> return value
                Nothing -> do
                    putStrLn "Error evaluating expression."
                    return (IntVal 0) -- or any default value you prefer
            let newVars = updateVars var val (vars st)
            let st' = LState {vars = newVars}
            repl st'

process st (Print e) = do
    val <- case eval (vars st) e of
        Just value -> return value
        Nothing -> do
            putStrLn "Error evaluating expression."
            return (IntVal 0) -- or any default value you prefer
    case val of
        IntVal n -> putStrLn $ show n
        StrVal s -> putStrLn s
    repl st

-- | Trims leading and trailing whitespace from a string.
-- @param String to trim.
-- @return Trimmed string.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | The main loop of the REPL, continuously reading, evaluating, and printing results
-- based on user input until the 'quit' command is received.
-- @param st The current state of the REPL.
repl :: LState -> IO ()
repl st = do
    putStr "> "
    hFlush stdout  -- Ensure the prompt is displayed immediately.
    inp <- getLine
    let trimmedInp = trim inp  -- Trim whitespace for cleaner command processing.
    if trimmedInp == "quit"
    then putStrLn "Exiting..."  -- Handle the quit command.
    else case parse pCommand trimmedInp of  -- Parse and process the command.
            [(cmd, "")] -> process st cmd
            _ -> do putStrLn "Parse error"
                    repl st  -- Loop back for new input on parse error.
