module Heuristics.Frequency (
  charFrequencyMap,
  frequencyChoose
) where

import Data.Map.Strict as Map
import Data.List (nub, sortBy)

-- given a list of words, return a normalized frequency map of the letters
charFrequencyMap :: [String] -> Map.Map Char Double -> Map.Map Char Double
charFrequencyMap all_words freq_map = Map.map (/ total_letter_count) letter_to_count
  where
    letter_to_count = Prelude.foldl (Prelude.foldl insert_op) freq_map all_words
    insert_op acc char = Map.insertWith (+) char 1.0 acc
    total_letter_count = 5 * fromIntegral (length all_words)

frequencyChoose :: [String] -> Map.Map Char Double -> String
frequencyChoose word_list freq_map = fst $ word_to_score !! 0
  where word_to_score = sortedWordScores word_list freq_map

sortedWordScores :: [String] -> Map.Map Char Double -> [(String, Double)]
sortedWordScores word_list freq_map = sortBy (\(_, a) (_, b) -> compare b a) word_scores
  where
    word_scores = Prelude.map (\word -> (word, wordScore word freq_map)) word_list

wordScore :: String -> Map.Map Char Double -> Double
wordScore word freq_map = score / repeat_letter_count
  where
    score = Prelude.foldl (\acc ch -> acc + (freq_map Map.! ch)) 0.0 word
    repeat_letter_count = 5 - fromIntegral (length $ nub word) + 1