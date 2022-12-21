#!/bin/bash

stack build
mkdir data/stats

################################################
# parMap Experiment for all words in hard_words
################################################
for i in {1..3}
do
  while IFS= read -r line; do
    for cpu_num in {1..8}
    do
      (stack exec wordleSolver-exe -- -m "entropy" -p "parMap" -f 50 -e 30 -w $line +RTS -ls -N$cpu_num -s) 2>&1 | grep "Total" | cut -c 30,31,32,33,34 >> data/stats/wordleSolver-parMap-N$cpu_num-T$i.txt
    done
  done < ./data/hard_words.txt
done


################################################
# parChunkList Experiment for all words in hard_words
################################################
for i in {1..3}
do
  while IFS= read -r line; do
    for cpu_num in {1..8}
    do
      (stack exec wordleSolver-exe -- -m "entropy" -p "parListChunk" -f 50 -e 30 -w $line +RTS -ls -N$cpu_num -s) 2>&1 | grep "Total" | cut -c 30,31,32,33,34 >> data/stats/wordleSolver-parListChunk-N$cpu_num-T$i.txt
    done
  done < ./data/hard_words.txt
done

# # ###################################################
# # # chunkSize Experiment for parListChunk using baker
# # ###################################################
for i in {1..10}
do
  for cpu_num in {1..8}
  do
    for chunk_sz in {16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536}
    do
      entropy_chunk=$chunk_sz
      # filter chunk is 1.5 times entropy chunk
      filter_chunk=$(( ($entropy_chunk * 3) / 2 ))
      # for filter_chunk in {32,64,128,256,512,1024,2048}
      # do
      printf "$cpu_num, $entropy_chunk, $filter_chunk, " >> ./data/stats/wordleSolverParListChunkT$i.csv
      (stack exec wordleSolver-eenxe -- -m "entropy" -p "parListChunk" -f $filter_chunk -e $entropy_chunk -w "baker" +RTS -ls -N$cpu_num -s) 2>&1 | grep "Total" | cut -c 30,31,32,33,34 >> ./data/stats/wordleSolverParListChunkT$i.csv
        # (stack exec wordleSolver-exe "baker" "parListChunk" $entropy_chunk $filter_chunk -- +RTS -ls -N$cpu_num -s) 2>&1 | grep "Total" | cut -c 30,31,32,33,34 >> ./data/stats/wordleSolverParListChunkT$i.csv
        # echo "$cpu_num, $entropy_chunk, $filter_chunk, $time" >> ./data/stats/wordleSolverParListChunk.csv
      # done
    done
  done
done

################################################
# Fold experiment with parChunkList for all words in hard_words
################################################
for i in {1..3}
do
  while IFS= read -r line; do
    for cpu_num in {1..8}
    do
      (stack exec wordleSolver-exe -- -m "entropy" -p "parListChunk" -f 50 -e 30 -w $line +RTS -ls -N$cpu_num -s) 2>&1 | grep "Total" | cut -c 30,31,32,33,34 >> data/stats/wordleSolver-fold-parListChunk-N$cpu_num-T$i.txt
      # (stack exec wordleSolver-exe $line "parListChunk" 50 30 -- +RTS -ls -N$cpu_num -s) 2>&1 | grep "Total" | cut -c 30,31,32,33,34 >> data/stats/wordleSolver-parListChunk-N$cpu_num-T$i.txt
    done
  done < ./data/hard_words.txt
done