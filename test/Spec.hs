{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import ENVCAP.Interpreter
import ENVCAP.Source.Errors
import ENVCAP.Syntax

import Test.Hspec
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), replaceExtension)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text, unpack)  
import Control.Monad (forM)

type TestName = String
type Code = String
type ExpectedOutput = String
type ExpectedError = String

type TestCase = (TestName, Code, TestType)
data TestType = DoPass ExpectedOutput
              | DoErr ExpectedError

rstrip :: String -> String
rstrip = dropWhileEnd isSpace

getPassFileTests :: FilePath -> IO [TestCase]
getPassFileTests dir = do
    files <- listDirectory dir
    forM (filter (endsWith ".ep") files) $ \file -> do  
        let epFile = dir </> file
        let passFile = replaceExtension epFile ".pass"
        code <- readFile epFile
        passExists <- doesFileExist passFile
        if passExists
            then do
                expectedOutput <- readFile passFile
                return (file, code, DoPass (rstrip expectedOutput))
            else
                error $ "Expected output file does not exist: " ++ passFile  

getErrFileTests :: FilePath -> IO [TestCase]
getErrFileTests dir = do
    files <- listDirectory dir
    forM (filter (endsWith ".ep") files) $ \file -> do
        let epFile = dir </> file
        let errFile = replaceExtension epFile ".err"
        code <- readFile epFile
        errExists <- doesFileExist errFile
        if errExists
            then do
                expectedErr <- readFile errFile
                return (file, code, DoErr (rstrip expectedErr))
            else
                error $ "Expected error file does not exist: " ++ errFile  

endsWith :: String -> String -> Bool
endsWith ext file = reverse ext == take (length ext) (reverse file)

hardcodedTests :: [TestCase]
hardcodedTests =
    [ 
    -- adding the module declaration is annoying, either make a default declaration, or maybe have
    -- a function to wrap code in a placeholder module
    ]

runTest :: TestCase -> SpecWith ()
runTest (name, code, testType) = it ("should interpret " ++ name ++ " correctly") $ do
    case interpreter code of
        Left err -> case testType of
                        DoErr expectedErr -> return () -- TODO
                        DoPass _ -> return () -- TODO
        Right val -> case testType of
                         DoPass expectedOutput -> show val `shouldBe` expectedOutput
                         DoErr _ -> return () -- TODO

main :: IO ()
main = do
    passFileTests <- getPassFileTests "test/tests/DoPass"
    errFileTests <- getErrFileTests "test/tests/DoErr"  
    let allTests = hardcodedTests ++ passFileTests ++ errFileTests
    hspec $ do
        describe "Interpreter Tests" $
            mapM_ runTest allTests
