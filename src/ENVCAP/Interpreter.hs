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
import ENVCAP.Core.PrettyPrint (prettyPrintExp)

translate :: Tm -> Either SourceTypeError Elab
translate tm = do
                expandedTm <- case expandAlias Source.TUnit tm of
                                Just tm' -> Right tm'
                                Nothing  -> Left $ generateError Source.TUnit tm "Failed to expand type aliases." ""

                desugaredTm <- case desugar expandedTm of
                                Just tm' -> Right tm'
                                Nothing  -> Left $ generateError Source.TUnit tm "Failed to desugar the term." ""

                elaborateInfer Source.TUnit desugaredTm

-- unescape :: String -> String
-- unescape [] = []
-- unescape ('\\' : 'n' : xs) = '\n' : unescape xs  -- Replace `\n` with newline
-- unescape ('\\' : '\\' : xs) = '\\' : unescape xs -- Replace `\\` with `\`
-- unescape ('\\' : '\"' : xs) = '\"' : unescape xs -- Replace `\"` with `"`
-- unescape (x : xs) = x : unescape xs

main :: String -> IO ()
main filePath = do
                    content <- catchIOError (readFile filePath) handleError
                    case parseSource content of
                        Nothing  -> putStrLn $ "Parse Error: " ++ show content ++ "\n"
                        Just res -> 
                            do  putStrLn $ "Parsing:        SUCCESS " ++ show res ++ "\n"
                                case translate res of
                                    Right (ty, tm)     -> 
                                        do  putStrLn $ "Elaboration:    SUCCESS \n" ++ show tm ++ "\n\nSource Type:    " ++ show ty ++ "\n"
                                            case infer Core.TUnit tm of
                                                Right ty     -> 
                                                    do  putStrLn $ "Type Check:     SUCCESS " ++ show ty ++ "\n"
                                                        case eval VUnit tm of
                                                            Just val    -> putStrLn $ "Evaluation:     SUCCESS " ++ show val ++ "\n"
                                                            _           -> putStrLn $ "Evaluation FAILED" ++ "\n"
                                                Left err      -> putStrLn $ "Type Check: FAIL" ++ show err ++ "\n"
                                    Left (STypeError err)     -> 
                                        putStrLn $ "Elaboration:        FAILED " ++ show res ++ "\n \n" ++ unescape (show err) ++"\n"

execute :: String -> Maybe Value
execute content = do 
                    case parseSource content of
                        Just tm -> 
                            case translate tm of
                                Right (ty, tm') -> 
                                    case infer Core.TUnit tm' of
                                        Right ty'  -> if ty'' == ty' then eval VUnit tm' else Nothing
                                                        where Just ty'' = elaborateTyp ty
                                        _          -> Nothing
                                Left err -> Nothing
                        Nothing -> Nothing
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
