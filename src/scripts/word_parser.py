def main():
  for desired_len in range(10, 21):
    filename = "../../data/dictionary/words.txt"
    out_filename = "../../data/dictionary/" + str(desired_len) + "_letter_words.txt"
    
    with open(filename) as f:
      for line in f:
        word = line.split(" ")[0]
        word = word.replace("\r", "")
        word = word.replace("\n", "")
        word = word.lower()
        if len(word) != desired_len or not word.isalpha():
          continue
        # print(word)
        with open(out_filename, "a") as out_f:
          out_f.write(word + "\n")

if __name__ == "__main__":
  main()
