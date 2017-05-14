module Main where


import System.IO ( stdin, hGetContents )
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )

import Interpreter
import LexGrammar
import ParGrammar
import PrintGrammar

import Reconstruction ( testWithBuiltins )
import Simplifier ( simplify )

import ErrM
import AbsGrammar ( Program )

import Paths_interpreteur

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

runFile :: ([Token] -> Err Program) -> String -> IO b
runFile p f = readFile f >>= run p

parse :: (Show b, Print b) => ([Token] -> Err b) -> String -> IO b
parse p s = let ts = myLLexer s in case p ts of
           Bad message  -> do putStrLn "\nParse              Failed...\n"
                              putStrLn "Tokens:"
                              putStrLn $ show ts
                              putStrLn message
                              exitFailure
           Ok  tree -> return tree

run :: ([Token] -> Err Program) -> String -> IO b
run p s = do
  builtinsPath <- getDataFileName "data/builtins.hs"
  builtinsFile <- readFile builtinsPath
  builtinsTree <- parse p builtinsFile
  let sBuiltinsTree = simplify builtinsTree
  tree <- parse p s
  let sTree = simplify tree
  putStrLn $ show $ testWithBuiltins sBuiltinsTree sTree
  putStrLn $ show (interpretWithBuiltins sBuiltinsTree sTree)
  exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (file)          Parse content of files verbosely."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run pProgram
    [path] -> runFile pProgram path
    _ -> do
            putStrLn "\nIncorrect arguments"
            exitFailure





