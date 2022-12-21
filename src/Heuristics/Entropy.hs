{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Heuristics.Entropy (
  entropyChoose,
) where
import qualified Data.Map as Map
import Data.Set as Set ( Set, insert, member, empty )
import Control.DeepSeq (NFData)
import Control.Monad.Par (Par, parMap, runPar, spawnP, get)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import qualified Data.Traversable as T

entropyChoose :: [String] -> [String] -> String -> Int -> String
entropyChoose valid_words all_words par_method entropy_chunk_sz
  | length valid_words <= 4 = head valid_words
  | par_method == "seq" = 
      entropyChooseMap valid_words all_words freq_list_map word_so_far
  | par_method == "parMap" = 
      entropyChooseMapPar valid_words all_words freq_list_map word_so_far entropy_chunk_sz
  | par_method == "parListChunk" = 
      entropyChooseMapChunkPar valid_words all_words freq_list_map word_so_far entropy_chunk_sz
  | otherwise = 
      entropyChooseMap valid_words all_words freq_list_map word_so_far
  where freq_list_map = buildFreqListMap valid_words
        -- freq_list_map = buildFreqListMapPar valid_words
        word_so_far = foldr1 (zipWith (\x y -> if x == y then x else '0')) valid_words

-- Sequential implementation
-- Version 1: recursive version
entropyChooseRec :: [String] -> [String] -> Map.Map Char [Int] -> String -> Double -> String -> String
entropyChooseRec _ [] _ _ _ best_word = best_word -- 2nd param is all_words
entropyChooseRec valid_words (w:ws) freqListMap word_so_far best_entropy best_word
  | length valid_words <= 2 = valid_words !! 0
  | otherwise =
    let diffEntropy = getDiffEntropyForWord valid_words w 0 freqListMap Set.empty word_so_far 0
    in if diffEntropy >= best_entropy
       then entropyChooseRec valid_words ws freqListMap word_so_far diffEntropy w
       else entropyChooseRec valid_words ws freqListMap word_so_far best_entropy best_word

-- Version 2: map & fold version
entropyChooseMap :: [String] -> [String] -> Map.Map Char [Int] -> String -> String
entropyChooseMap valid_words all_words freqListMap word_so_far =
    let diffEntropies = map (\word -> (word,
          getDiffEntropyForWord valid_words word 0 freqListMap Set.empty word_so_far 0))
          all_words
        max_word = foldr1 (\(w1, e1) (w2, e2) -> if e1 >= e2 then (w1, e1) else (w2, e2)) diffEntropies
    in fst max_word

-- Parallel version of entropyChooseMap
-- Method 1: parMap (Par monad)
parMapChunk :: (NFData b) => Int -> (a -> b) -> [a] -> Par [b]
parMapChunk n f xs = fmap concat $ parMap (map f) $ chunk n xs
  where chunk _ [] = []
        chunk c_n c_xs = as : chunk c_n bs
          where (as, bs) = splitAt c_n c_xs

entropyChooseMapPar :: [String] -> [String] -> Map.Map Char [Int] -> String -> Int -> String
entropyChooseMapPar valid_words all_words freqListMap word_so_far entropy_chunk_sz =
    let diffEntropies = runPar $ parMapChunk entropy_chunk_sz (\word -> (word,
          getDiffEntropyForWord valid_words word 0 freqListMap Set.empty word_so_far 0))
          all_words
        max_word = foldr1 (\(w1, e1) (w2, e2) -> if e1 >= e2 then (w1, e1) else (w2, e2)) diffEntropies
    in fst max_word

-- Method 2: parListChunk
entropyChooseMapChunkPar :: [String] -> [String] -> Map.Map Char [Int] -> String -> Int -> String
entropyChooseMapChunkPar valid_words all_words freqListMap word_so_far entropy_chunk_sz =
    let diffEntropies = map (\word -> (word,
          getDiffEntropyForWord valid_words word 0 freqListMap Set.empty word_so_far 0))
          all_words `using` parListChunk entropy_chunk_sz rdeepseq
        max_word = foldr1 (\(w1, e1) (w2, e2) -> if e1 >= e2 then (w1, e1) else (w2, e2)) diffEntropies
    in fst max_word

getDiffEntropyForWord :: [String] -> String -> Int -> Map.Map Char [Int] -> 
  Set Char -> String -> Double -> Double
getDiffEntropyForWord valid_words word wordIdx freqListMap seenChars wordSoFar diffEntropy
  | wordIdx >= 5 = diffEntropy
  | otherwise =
    let ch = word !! wordIdx
        greens = freqListMap Map.! ch !! wordIdx
        yellows = if ch `Set.member` seenChars
            then 0
            else let mask = [c /= ch | c <- wordSoFar]
                 in sum([freqListMap Map.! ch !! j | j <- [0..length wordSoFar - 1], mask !! j]) - greens
        seenChars' = Set.insert ch seenChars
        greys = length valid_words - greens - yellows
        dist = [greens, yellows, greys]
        dist' = [fromIntegral x / fromIntegral (sum dist) | x <- dist]
        diffEntropy' = diffEntropy + entropy dist'
    in getDiffEntropyForWord valid_words word (wordIdx + 1) freqListMap seenChars' wordSoFar diffEntropy'

entropy :: (Eq a, Floating a) => [a] -> a
entropy dist = -sum [x * log2_safe x | x <- dist]
  where log2_safe x = if x == 0.0 then 0 else logBase 2 x

-- sequential frequency map building
buildFreqListMap :: [String] -> Map.Map Char [Int]
buildFreqListMap valid_words = Map.fromList [(ch, buildFreqList valid_words ch) | ch <- ['a'..'z']]
  where buildFreqList (w:ws) ch = zipWith (+) (buildFreqListWord w ch) (buildFreqList ws ch)
        buildFreqList [] _ = [0,0,0,0,0]
        buildFreqListWord word ch = [if x == ch then 1 else 0 | x <- word]

-- parallel frequency map building
buildFreqListMapPar :: [String] -> Map.Map Char [Int]
buildFreqListMapPar valid_words = Map.fromList [(ch, buildFreqList valid_words ch) | ch <- ['a'..'z']]
  where buildFreqList vw ch = runPar $ parAcc [0,0,0,0,0] [buildFreqListWord x ch | x <- vw]
        buildFreqListWord word ch = [if x == ch then 1 else 0 | x <- word]

        parAcc _ [x] = return x
        parAcc x xs = parAcc x $ runPar $ do
          let res = acc x xs
          seq_res <- T.sequence res
          T.mapM get seq_res

        acc x (y1 : y2 : ys) = spawnP (zipWith (+) y1 y2) : acc x ys
        acc x (y1 : _) = [spawnP (zipWith (+) y1 x)]
        acc _ [] = []
