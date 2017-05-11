module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Interpreter
import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar

import Reconstruction ( test )
import Simplifier ( simplify )

import ErrM

import Paths_interpreteur

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

-- runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

-- parse :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
parse v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          return tree

run v p s = do
  builtinsPath <- getDataFileName "data/builtins.hs"
  builtinsFile <- readFile builtinsPath
  putStrLn "parsing builtins"
  builtinsTree <- parse v p builtinsFile
  let sBuiltinsTree = simplify builtinsTree
  putStrLn "parsing file"
  tree <- parse v p s
  let sTree = simplify tree
  putStrLn $ show (interpretWithBuiltins sBuiltinsTree sTree)
  exitSuccess

-- showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (file)          Parse content of files verbosely."
    , "  -s (file)       Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run 2 pProgram
    ["-s", path] -> runFile 0 pProgram path
    [path] -> runFile 2 pProgram path
    _ -> do
            putStrLn "\nIncorrect arguments"
            exitFailure





