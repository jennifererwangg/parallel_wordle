module Heuristics.Minimax (
  minimaxChoose
) where

import qualified Data.Map as Map
import WordleGame ( getWordleResponse )
import Data.Maybe (fromMaybe)

minimaxChoose :: [String] -> String
minimaxChoose valid_words =
  if length valid_words <= 2 then head valid_words
  else minimize valid_words 1000000 "" 1000000

minimize :: [String] -> Int -> String -> Int -> String
minimize [] _ min_word _ = min_word
minimize valid_words@(w:ws) beta min_word min_bucket_size =
  minimize ws beta' min_word' min_bucket_size'
  where max_bucket_size = maximize w valid_words beta Map.empty 0
        min_bucket_size' = min min_bucket_size max_bucket_size
        min_word' = if max_bucket_size < min_bucket_size then w else min_word
        beta' = min beta min_bucket_size'

maximize :: String -> [String] -> Int -> Map.Map String Int -> Int -> Int
maximize _ [] _ _ max_bucket_size = max_bucket_size
maximize player_guess (potential_answer:vws) beta pat_to_cnt max_bucket_size =
    let pattern = getWordleResponse player_guess potential_answer
        count = fromMaybe 0 $ Map.lookup pattern pat_to_cnt
        new_count = count + 1
        pat_to_cnt' = Map.insert pattern new_count pat_to_cnt
        max_bucket_size' = max new_count max_bucket_size
    in if max_bucket_size' >= beta then max_bucket_size'
       else maximize player_guess vws beta pat_to_cnt' max_bucket_size'
