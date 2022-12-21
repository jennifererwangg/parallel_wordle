module Main (main) where

import Parser
import Player
import Interactive ( interactiveLoop )

import System.Environment (getArgs)
import Control.Parallel.Strategies (using, parList, rdeepseq)
import System.Console.ParseArgs ()
import System.Console.GetOpt
    ( getOpt,
      ArgDescr(NoArg, ReqArg),
      ArgOrder(Permute),
      OptDescr(..) )
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  opts <- wordleOpts args

  valid_words <- fileToWordList "data/answerlist.txt" 
  all_words <- fileToWordList "data/wordlist.txt"

  case opts of
    (Flags {help = True}, _) -> putStrLn usageMsg
    (Flags {interactive = True}, _) -> interactiveLoop valid_words all_words
    (Flags {word = "", method = m, filter_chunk_sz = f_sz, 
            entropy_chunk_sz = e_sz, par_method = pm}, _) -> do
      let arrEntropy = Prelude.map (\w ->
            playAndGetList m w valid_words all_words f_sz e_sz pm)
            valid_words `using` parList rdeepseq
      printGameRes arrEntropy valid_words
    (Flags {word = w, method = m, filter_chunk_sz = f_sz, 
            entropy_chunk_sz = e_sz, par_method = pm}, _) -> do
      let answer = playAndGetList m w valid_words all_words f_sz e_sz pm
      print answer
      -- uncomment the following when doing performance testing
      -- _ <- playAndGetList m w valid_words all_words f_sz e_sz pm `seq` return ()
      -- return ()

data Flags = Flags
  {
    interactive :: Bool,
    method :: String,
    par_method :: String,
    filter_chunk_sz :: Int,
    entropy_chunk_sz :: Int,
    word :: String,
    help :: Bool
  } deriving Show

defaultFlags :: Flags
defaultFlags = Flags
  {
    interactive = False,
    method = "entropy",
    par_method = "parMap",
    filter_chunk_sz = 50,
    entropy_chunk_sz = 30,
    word = "",
    help = False
  }

flags :: [OptDescr (Flags -> Flags)]
flags =
  [ Option ['i'] ["interactive"] (ReqArg (\f opts -> 
      opts { interactive = readInteractive f }) "Bool") "Interactive mode"
  , Option ['m'] ["method"] (ReqArg (\f opts -> 
      opts { method = readMethod f }) "String") "Heuristic method"
  , Option ['p'] ["parMethod"] (ReqArg (\f opts -> 
      opts { par_method = readParMethod f }) "String") "Parallel method"
  , Option ['f'] ["filterChunk"] (ReqArg (\f opts -> 
      opts { filter_chunk_sz = readFilterChunkSz f }) "Int") "Filter chunk size"
  , Option ['e'] ["entropyChunk"] (ReqArg (\f opts -> 
      opts { entropy_chunk_sz = readEntropyChunkSz f }) "Int") "Entropy chunk size"
  , Option ['w'] ["word"] (ReqArg (\f opts -> 
      opts { word = f }) "String") "Guess word"
  , Option ['h'] ["help"] (NoArg (\opts -> 
      opts { help = True })) "Show help"
  ]

readInteractive :: String -> Bool
readInteractive s = case readMaybe s of
  Just b -> b
  Nothing -> error "ERROR: unrecognized option -i [Bool]"

readMethod :: String -> String
readMethod s = case s of
  "entropy" -> "entropy"
  "frequency" -> "frequency"
  "minimax" -> "minimax"
  _ -> error "ERROR: unrecognized option -m [entropy | frequency | minimax]"

readParMethod :: String -> String
readParMethod s = case s of
  "seq" -> "seq"
  "parMap" -> "parMap"
  "parListChunk" -> "parListChunk"
  _ -> error "ERROR: unrecognized option -p [seq | parMap | parListChunk]"

readFilterChunkSz :: String -> Int
readFilterChunkSz s = case readMaybe s of
  Just i -> i
  Nothing -> error "ERROR: unrecognized option -f [Int]"

readEntropyChunkSz :: String -> Int
readEntropyChunkSz s = case readMaybe s of
  Just i -> i
  Nothing -> error "ERROR: unrecognized option -e [Int]"

wordleOpts :: [String] -> IO (Flags, [String])
wordleOpts args =
  case getOpt Permute flags args of
    (xs, n, []) -> return (foldl (flip id) defaultFlags xs, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageMsg))

usageMsg :: [Char]
usageMsg =
  "Usage: wordleSolver [OPTION...]" ++ "\n" ++
  "  -i Bool    --interactive=Bool  Interactive mode" ++ "\n" ++
  "  -m String  --method=String     Heuristic method" ++ "\n" ++
  "  -p String  --parMethod=String  Parallel method" ++ "\n" ++
  "  -f Int     --filterChunk=Int   Filter chunk size" ++ "\n" ++
  "  -e Int     --entropyChunk=Int  Entropy chunk size" ++ "\n" ++
  "  -w String  --word=String       Guess word" ++ "\n" ++
  "  -h         --help              Show help"