module Main where


import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )

import Interpreter
import LexGrammar
import ParGrammar
import PrintGrammar

import Reconstruction ( testWithBuiltins )
import Simplifier ( simplify )

import ErrM
import qualified AbsGrammar as AG
import qualified SimpleGrammar as SG

import Paths_interpreteur

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

printError :: String -> IO ()
printError = hPutStrLn stderr

runFile :: ([Token] -> Err AG.Program) -> String ->
           (SG.Program -> SG.Program -> Either String String) -> IO b
runFile p f r = readFile f >>= run p r

parse :: (Show b, Print b) => ([Token] -> Err b) -> String -> IO b
parse p s = let ts = myLLexer s in case p ts of
           Bad message  -> do printError "Parsing failed"
                              printError "Tokens"
                              printError $ show ts
                              printError message
                              exitFailure
           Ok  tree -> return tree

run :: ([Token] -> Err AG.Program) -> (SG.Program -> SG.Program -> Either String String) ->
       String -> IO b
run p interpreterFunc s = do
  builtinsPath <- getDataFileName "data/builtins.hs"
  builtinsFile <- readFile builtinsPath
  builtinsTree <- parse p builtinsFile
  let sBuiltinsTree = simplify builtinsTree
  tree <- parse p s
  let sTree = simplify tree
  either 
    (\x -> do printError x; exitFailure)
    (\x -> putStrLn x)
    (interpreterFunc sBuiltinsTree sTree)
  exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (file)          Type check and interpret the file."
    , "  -t (file)       Only type check the file and print the type of main function."
    , "  -d (file)       (Dynamic typing) only interpret the file without type checking."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [path] -> runFile pProgram path checkInterpret
    ["-t", path] -> runFile pProgram path typeCheck
    ["-d", path] -> runFile pProgram path dynInterpret
    _ -> do
            printError "Incorrect arguments"
            exitFailure

  where 
    checkInterpret :: SG.Program -> SG.Program -> Either String String
    checkInterpret sBuiltinsTree sTree = do
      _ <- typeCheck sBuiltinsTree sTree
      dynInterpret sBuiltinsTree sTree
   
    typeCheck :: SG.Program -> SG.Program -> Either String String
    typeCheck builtinsTree tree = do
      res <- testWithBuiltins builtinsTree tree
      return $ show res

    dynInterpret :: SG.Program -> SG.Program -> Either String String
    dynInterpret builtinsTree tree =
      case interpretWithBuiltins builtinsTree tree of
        Bad s -> Left s
        Ok s -> Right $ show s

