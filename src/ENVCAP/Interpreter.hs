module ENVCAP.Interpreter where

import System.IO.Error (catchIOError)
import ENVCAP.Parser.Parser (parseMain)
import ENVCAP.Parser.Happy (parseSource)
import ENVCAP.Source.Desugar
import ENVCAP.Source.Elaboration
import ENVCAP.Source.Syntax as Source
import ENVCAP.Core.TypeChecker (infer)
import ENVCAP.Core.Syntax as Core
import ENVCAP.Core.Evaluator (eval)

-- let filePath = "examples/Source/Arithmetic.ep"

translate :: Tm -> Maybe Exp
translate tm    = elaborate =<< desugar =<< expandAlias Source.TUnit tm
    
main :: String -> IO ()
main filePath = do
    content <- catchIOError (readFile filePath) handleError
    case parseSource content of
        Nothing  -> putStrLn $ "Parse Error: " ++ show content ++ "\n"
        Just res -> 
            do  putStrLn $ "Parsing:        SUCCESS " ++ show res ++ "\n"
                case translate res of
                    Nothing -> putStrLn $ "Desugar:        FAILED " ++ show res ++ "\n"
                    Just tm     -> 
                        do  putStrLn $ "Elaboration:    SUCCESS " ++ show tm ++ "\n"
                            case infer Core.TUnit tm of
                                Right ty     -> 
                                    do  putStrLn $ "Type Check:     SUCCESS " ++ show ty ++ "\n"
                                        case eval VUnit tm of
                                            Just val    -> putStrLn $ "Evaluation:     SUCCESS " ++ show val ++ "\n"
                                            _           -> putStrLn $ "Evaluation FAILED" ++ "\n"
                                Left err      -> putStrLn $ "Type Check: FAIL" ++ show err ++ "\n"

execute :: String -> Maybe Value
execute content = do 
                    res <- translate =<< parseSource content
                    case infer Core.TUnit res of
                        Right ty   -> eval VUnit res
                        _          -> Nothing

examples :: [String]
examples = ["Anonymous.ep", 
            "Arithmetic.ep",
            "CheckPrime.ep",
            "Conditional.ep",
            "Factorial.ep",
            "Fibonacci.ep",
            "FirstClassFunction.ep",
            "Functions.ep",
            "TyAliases.ep"]

tester :: [String] -> IO()
tester []       = putStrLn "Completed."
tester (x:xs)   = do
                    content <- catchIOError (readFile ("examples/Source/" ++ x)) handleError
                    case execute content of
                        Just v      -> putStrLn $ "SUCCESS: " ++ show x
                        _           -> putStrLn $ "FAILED:  " ++ show x
                    tester xs

handleError :: IOError -> IO String
handleError _ = return "Error: Unable to read file."
