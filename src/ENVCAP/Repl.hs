module ENVCAP.Repl where
import System.Console.Haskeline
import ENVCAP.Source.Errors 
import ENVCAP.Interpreter (interpreter)

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
        Just input -> do    case eval' input of
                                Left err -> outputStrLn $ "Error: " ++ show err
                                Right result -> outputStrLn result
                            loop
        Nothing -> outputStrLn "Goodbye!"


