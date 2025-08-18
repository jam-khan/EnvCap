module ENVCAP.Repl where

import ENVCAP.Interpreter (interpreter)
import ENVCAP.Source.Errors
import System.Console.Haskeline

eval' :: String -> Either InterpreterError String
eval' input = do
    res <- interpreter input
    return $ show res

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "> "
        case minput of
            Just "quit" -> outputStrLn "Goodbye!"
            Just "exit" -> outputStrLn "Goodbye!"
            Just input -> do
                case eval' input of
                    Left err -> outputStrLn $ "Error: " ++ show err
                    Right result -> outputStrLn result
                loop
            Nothing -> outputStrLn "Goodbye!"
