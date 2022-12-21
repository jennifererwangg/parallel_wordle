module Player (
  playAndGetList,
  filterValidWords
) where
import qualified Data.Map.Strict as Map

import WordleGame ( getWordleResponse )
import Heuristics.Frequency ( charFrequencyMap, frequencyChoose )
import Heuristics.Entropy ( entropyChoose )
import Heuristics.Minimax ( minimaxChoose )
import Control.DeepSeq (NFData)
import Control.Monad.Par (Par, parMap, runPar)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)


playAndGetList :: String -> String -> [String] -> [String] -> Int -> Int -> String -> [String]
playAndGetList method answer valid_words all_words filter_chunk_sz entropy_chunk_sz par_method
  | method == "frequency" = 
      playFreq answer valid_words freq_map []
  | method == "entropy" = 
      playEntropy answer valid_words all_words [] filter_chunk_sz entropy_chunk_sz par_method
  | method == "minimax" = 
      playMinimax answer valid_words (length valid_words) []
  | otherwise = 
      error( "ERROR: Player.hs: playAndGetList: invalid method. " ++ 
             "Please use 'frequency' or 'entropy' or 'minimax'")
  where freq_map = charFrequencyMap all_words Map.empty

playFreq :: String -> [String] -> Map.Map Char Double -> [String] -> [String]
playFreq answer valid_words freq_map choices 
  | guess == answer = reverse $ guess : choices
  | otherwise = playFreq answer new_valid_words freq_map (guess : choices)
  where new_valid_words = filterValidWords valid_words guess wordle_response
        guess = frequencyChoose valid_words freq_map
        wordle_response = getWordleResponse guess answer

playMinimax :: String -> [String] -> Int -> [String] -> [String]
playMinimax answer valid_words orig_valid_len choices 
  | guess == answer = reverse $ guess : choices
  | otherwise = playMinimax answer new_valid_words orig_valid_len (guess : choices)
  where new_valid_words = filterValidWords valid_words guess wordle_response
        -- hardcode the first guess to speed things up
        guess = if length valid_words == orig_valid_len then "raise"
                else minimaxChoose valid_words
        wordle_response = getWordleResponse guess answer

playEntropy :: String -> [String] -> [String] -> [String] -> Int -> Int -> String -> [String]
playEntropy answer valid_words all_words choices filter_chunk_sz entropy_chunk_sz par_method
  | guess == answer = reverse $ guess : choices
  | otherwise = 
      playEntropy answer new_valid_words all_words 
        (guess : choices) filter_chunk_sz entropy_chunk_sz par_method
    where new_valid_words
            | par_method == "parListChunk" = 
              parFilterValidWordsChunk valid_words guess wordle_response filter_chunk_sz
            | par_method == "parMap" = 
              parFilterValidWords valid_words guess wordle_response filter_chunk_sz
            | par_method == "seq" =
              filterValidWords valid_words guess wordle_response
            | otherwise = 
              error ("ERROR: Player.hs: playEntropy: invalid par_method. " ++ 
                      "Please use 'parMap' or 'parMapChunk'")
          guess = entropyChoose valid_words all_words par_method entropy_chunk_sz
          wordle_response = getWordleResponse guess answer

-- sequential filterValidWords
filterValidWords :: [String] -> String -> String -> [String]
filterValidWords valid_words query wordle_response =
  filter (\word -> wordle_response == getWordleResponse query word) valid_words

-- parallel filterValidWords
-- Method 1: use parMap (par monad)
parFilter :: (NFData a) => Int -> (a -> Bool) -> [a] -> Par [a]
parFilter n f xs = fmap concat $ parMap (filter f) $ chunk n xs
  where chunk _ [] = []
        chunk c_n c_xs = as : chunk c_n bs
          where (as, bs) = splitAt c_n c_xs

parFilterValidWords :: [String] -> String -> String -> Int -> [String]
parFilterValidWords valid_words query wordle_response chunk_sz =
  runPar $ parFilter chunk_sz (\word -> wordle_response == getWordleResponse query word) valid_words

-- Method 2: use parListChunk
parFilterValidWordsChunk :: [String] -> String -> String -> Int -> [String]
parFilterValidWordsChunk valid_words query wordle_response chunk_sz=
  filter (\word -> wordle_response == getWordleResponse query word) valid_words 
    `using` parListChunk chunk_sz rdeepseq