module ENVCAP.Interpreter where

import System.IO.Error (catchIOError)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import ENVCAP.Parser.Parser (parseMain)
import ENVCAP.Parser.Happy (parseSource)
import ENVCAP.Source.Desugar
import ENVCAP.Source.Elaboration
import ENVCAP.Source.Syntax as Source
import ENVCAP.Core.TypeChecker (infer)
import ENVCAP.Core.Syntax as Core
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.PrettyPrint (prettyPrintExp, prettyPrintVal)

translate :: Tm -> Either SourceTypeError Elab
translate tm = do
                expandedTm <- case expandAlias Source.TUnit tm of
                                Just tm' -> Right tm'
                                Nothing  -> Left $ generateError Source.TUnit tm "Failed to expand type aliases." ""

                desugaredTm <- case desugar expandedTm of
                                Just tm' -> Right tm'
                                Nothing  -> Left $ generateError Source.TUnit tm "Failed to desugar the term." ""

                elaborateInfer Source.TUnit desugaredTm


repl :: Bool -> IO ()
repl debug = do
        putStr "> "
        hFlush stdout
        input <- getLine
        when (input /= ":q") $
            do
                case parseSource input of
                    Just res -> 
                        do  putStr $ debugCheck $ "Parsing:        SUCCESS " ++ show res ++ "\n\n"
                            case translate res of
                                Right (ty, tm)     -> 
                                    do  putStr $ debugCheck $ "Elaboration:    SUCCESS \n" ++ show tm ++ "\n\nSource Type:    " ++ show ty ++ "\n\n"
                                        case infer Core.TUnit tm of
                                            Right ty     -> 
                                                do  putStr $ debugCheck $ "Type Check:     SUCCESS " ++ show ty ++ "\n\n"
                                                    case eval VUnit tm of
                                                        Just val    -> do   putStr $ prettyPrintVal val ++ "\n\n"
                                                                            repl debug
                                                        _           -> do   putStr $ "Evaluation FAILED" ++ "\n\n"
                                                                            repl debug
                                            Left err      -> do 
                                                                putStr $ debugCheck $ "Type Check: FAIL" ++ show err ++ "\n\n"
                                                                repl debug
                                Left (STypeError err)     -> 
                                    do
                                        putStrLn (if debug then "Elaboration:        FAILED " ++ show res ++ "\n \n" ++ unescape (show err) ++"\n\n" else "Failed")
                                        repl debug
                    Nothing  -> do 
                                    putStr $ "Parse Error: " ++ show input ++ "\n\n"
                                    repl debug
            where debugCheck str = if debug then str else ""
                

-- Debug mode
run :: String -> IO ()
run filePath = do
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
                                                        Just val    -> putStrLn $ "Evaluation:     SUCCESS " ++ prettyPrintVal val ++ "\n"
                                                        _           -> putStrLn $ "Evaluation FAILED" ++ "\n"
                                            Left err      -> putStrLn $ "Type Check: FAIL" ++ show err ++ "\n"
                                Left (STypeError err)     -> 
                                    putStrLn $ "Elaboration:        FAILED " ++ show res ++ "\n \n" ++ unescape (show err) ++"\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> repl False  -- Start REPL without debug mode
        ["--debug"] -> repl True   -- Start REPL with debug mode
        [filePath]  -> do          -- Interpret a file
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
        _           -> putStrLn "Usage: interpreter [--debug] [file]"

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
