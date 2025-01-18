module ENVCAP.Interpreter where

import System.IO.Error (catchIOError)
import ENVCAP.Parser.Parser (parseMain)
import ENVCAP.Source.Desugar (desugar)
import ENVCAP.Core.TypeChecker (infer)
import ENVCAP.Core.Syntax (Typ(..), Value (VUnit))
import ENVCAP.Core.Evaluator (eval)

-- let filePath = "examples/Source/Arithmetic.ep"
    
main :: String -> IO ()
main filePath = do
    content <- catchIOError (readFile filePath) handleError
    case parseMain content of
        Left    err -> putStrLn $ "Parse Error: " ++ show err
        Right   res -> do
                        putStrLn $ "Parsing:        SUCCESS " ++ show res
                        case desugar res of
                            Just tm     -> do
                                                putStrLn $ "Elaboration:    SUCCESS " ++ show tm
                                                case infer TUnit tm of
                                                    Just ty     -> do
                                                                    putStrLn $ "Type Check:     SUCCESS " ++ show ty 
                                                                    case eval VUnit tm of
                                                                        Just val    -> putStrLn ("Evaluation:     SUCCESS " ++ show val)
                                                                        _           -> putStrLn "Evaluation FAILED"
                                                    _           -> putStrLn "Type Check: FAIL"
                            _           -> putStrLn "Elaboration Failed"
                            

handleError :: IOError -> IO String
handleError _ = return "Error: Unable to read file."
