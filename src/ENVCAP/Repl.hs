module ENVCAP.Repl where
import System.Console.Haskeline

newtype InterpreterError = InterpreterFailed String deriving (Show)

eval' :: String -> Either InterpreterError String
eval' input = Right $ "You said: " ++ input

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Just "quit" -> outputStrLn "Goodbye!"
        Just "exit" -> outputStrLn "Goodbye!"
        Just input -> do    case eval' input of
                                Left err -> outputStrLn $ "Error: " ++ show err
                                Right result -> outputStrLn result
                            loop
        Nothing -> outputStrLn "Goodbye!"

main :: IO ()
main = do
  putStrLn "Welcome to the REPL! Type 'quit' or 'exit' to leave."
  repl
