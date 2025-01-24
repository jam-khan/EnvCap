module ENVCAP.Interpreter where

import System.IO.Error (catchIOError)
import ENVCAP.Parser.Parser (parseMain)
import ENVCAP.Parser.Happy (parseSource)
import ENVCAP.Source.Desugar
import ENVCAP.Core.TypeChecker (infer)
import ENVCAP.Core.Syntax (Typ(..), Value (VUnit))
import ENVCAP.Core.Evaluator (eval)

-- let filePath = "examples/Source/Arithmetic.ep"
    
main :: String -> IO ()
main filePath = do
    content <- catchIOError (readFile filePath) handleError
    case parseSource content of
        Nothing  -> putStrLn $ "Parse Error: " ++ show content ++ "\n"
        Just res -> do
                        putStrLn $ "Parsing:        SUCCESS " ++ show res ++ "\n"
                        case desugar res of
                            Nothing -> putStrLn $ "Desugar:        FAILED " ++ show res ++ "\n"
                            Just res' -> do putStrLn $ "Desugar:        SUCCESS " ++ show res' ++ "\n"
                                            case elaborate res' of
                                                Just tm     -> do
                                                                    putStrLn $ "Elaboration:    SUCCESS " ++ show tm ++ "\n"
                                                                    case infer TUnit tm of
                                                                        Right ty     -> do
                                                                                        putStrLn $ "Type Check:     SUCCESS " ++ show ty ++ "\n"
                                                                                        case eval VUnit tm of
                                                                                            Just val    -> putStrLn $ "Evaluation:     SUCCESS " ++ show val ++ "\n"
                                                                                            _           -> putStrLn $ "Evaluation FAILED" ++ "\n"
                                                                        Left err      -> putStrLn $ "Type Check: FAIL" ++ show err ++ "\n"
                                                _           -> putStrLn $ "Elaboration:    FAILED  " ++ show res' ++ "\n"

handleError :: IOError -> IO String
handleError _ = return "Error: Unable to read file."
