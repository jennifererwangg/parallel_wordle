module WordleGame (
  getWordleResponse
) where

import Data.Map.Strict as Map 

-- Given a user's guess and the correct answer, return the Wordle response
-- in the form of 0s, 1s, 2s, representing grey, yellow, and green, respectively.
getWordleResponse :: String -> String -> String
getWordleResponse query answer = response
  where letter_to_count = Map.fromListWith (+) $ Prelude.map (\x -> (x, 1)) answer
        green_and_grey = fillGreenAndGrey query answer letter_to_count
        -- Update letter_to_count map
        letter_to_count_updated = update_map letter_to_count green_and_grey query
        update_map ltc [] _ = ltc
        update_map ltc _ [] = ltc
        update_map ltc (x:xs) (q:qs) = if x == '2' then 
          update_map (Map.adjust (\c -> c - 1) q ltc) xs qs else update_map ltc xs qs
        -- Fill in the yellow letters
        response = fillYellow query letter_to_count_updated green_and_grey

fillGreenAndGrey :: String -> String -> Map.Map Char Int -> [Char]
fillGreenAndGrey [] _ _ = []
fillGreenAndGrey _ [] _ = []
fillGreenAndGrey (q:qs) (a:as) ltc 
  | q == a = '2' : fillGreenAndGrey qs as (Map.adjust (\x -> x - 1) a ltc)
  | otherwise = '0' : fillGreenAndGrey qs as ltc

fillYellow :: String -> Map.Map Char Int -> String -> [Char]
fillYellow [] _ _= []
fillYellow _ _ [] = []
fillYellow (q:qs) ltc (r:rs) | q `Map.member` ltc && r /= '2' && Map.findWithDefault 0 q ltc > 0
                                 = '1' : fillYellow qs (Map.adjust (\x -> x - 1) q ltc) rs
                              | otherwise = r : fillYellow qs ltc rs
