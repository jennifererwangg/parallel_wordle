-- An fun interactive player that can help you solve your NYT Wordle game of the day!
module Interactive (
  interactiveLoop,
) where

import Heuristics.Entropy (entropyChoose)
import Player (filterValidWords)

-- take user input in a loop
interactiveLoop :: [String] -> [String] -> IO ()
interactiveLoop valid_words all_words = do
  let guess_word = entropyChoose valid_words all_words "parMap" 50
  putStrLn ("Please enter \"" ++ guess_word ++ "\" into NYT's Wordle game.")
  -- 0 = grey, 1 = yellow, 2 = green
  putStrLn "Enter NYT's response in 0/1/2 format:"
  wordle_response <- getLine
  if not (isValidWordleResponse wordle_response)
    then putStrLn "Invalid response. Please try again." >> interactiveLoop valid_words all_words
    else
      let new_valid_words = filterValidWords valid_words guess_word wordle_response
       in case new_valid_words of
            [word] -> putStrLn ("Type \"" ++ word ++ "\" to win!")
            [] -> putStrLn "No valid words left. You win!"
            _ ->
              case wordle_response of
                "22222" -> putStrLn "No valid words left. You win!"
                _ -> interactiveLoop new_valid_words all_words

isValidWordleResponse :: String -> Bool
isValidWordleResponse response = length response == 5 && all (`elem` "012") response