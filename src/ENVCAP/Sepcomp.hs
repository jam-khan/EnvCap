module ENVCAP.Sepcomp where
import ENVCAP.Source.Errors (SeparateCompilationError (SepCompError), InterpreterError (InterpreterFailed))
import ENVCAP.Interpreter 
import Control.Exception 



-- Step 1: Add annotations
-- Step 2: Read interface files
-- Step 3: Read implementation files
-- Step 4: Combine implementation and interface as an annotation
-- Step 5: Read multiple files in order
--         Order reflects dependencies: File1 File2 (means that File2 depends on File1)
-- Step 6: Add three commands: 
--                  SeparateTypeCheck: File 1 ... File N (Check all files against header files)
--                  SeparateCompile:   File 1 ... File N (  1) Check 
--                                                          2) Load modules in environment as a box
--                                                          3) Elaborate to core and write in a file
--                                                          4) Each file gets a core generation
--                  envcap "E1.epc" ~~~~> Evaluates result
--

-- This is supposed to be a type basically
readInterface :: String -> Either SeparateCompilationError String
readInterface file = case parseCode file of
                        Left (InterpreterFailed err) -> Left $ SepCompError ("Couldn't read the interface loaded." ++ err) 
                        Right res                    -> Right $ show res

loadInterface :: String -> IO()
loadInterface interfaceFilePath = 
                do
                    result <- try (readFile interfaceFilePath) :: IO (Either IOException String)
                    case result of
                        Left ioexception -> putStrLn ("I/O error: " ++ show ioexception)
                        Right code       -> case readInterface code of
                                                Right res                  -> print res
                                                Left  (SepCompError err)   -> print err