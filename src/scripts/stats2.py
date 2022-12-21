def main():
  times = []

  for i in range(1, 9):
    times = []
    # filename = "../data/stats/wordleSolver-{}.txt".format(i)
    filename = "../data/runpar-stats/wordleSolver-{}.txt".format(i)
    with open(filename) as f:
      for line in f:
        time = line.split(" ")[0]
        times.append(float(time))
    print("Average time for CPU {}: {}".format(i, sum(times) / len(times)))

if __name__ == "__main__":
  main()