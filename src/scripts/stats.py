def main():
  total_turn_length = 0
  max_turn_length = 6
  hardest_words = []
  word_count = 0
  filename = "../data/sequential_data/entropy_game_output.txt"
  # filename = "../data/sequential_data/minimax_game_output.txt"
  # filename = "../data/sequential_data/freq_game_output.txt"

  with open(filename) as f:
    for line in f:
      word = line.split(":")[0].split('"')[1]
      turn_length = line.split("[")[1].split("]")[0]
      words = line.split(":")[1].split("[")[1].split("]")[0].split(",")
      words = [word.strip('"') for word in words]
      print(words)
      total_turn_length += len(words)
      if len(words) > max_turn_length:
        hardest_words.append(word)
      word_count += 1
  
  print("Average turn length: {}".format(total_turn_length / word_count))
  print("Games using > 6 turns: \n{}".format(hardest_words))
  print("len(hardest_words): {}".format(len(hardest_words)))

if __name__ == "__main__":
  main()