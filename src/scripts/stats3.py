def main():
  # make a list of 8 infinities
  times = [float('inf'), float('inf'), float('inf'), float('inf'), float('inf'), float('inf'), float('inf'), float('inf')]
  for i in range(1, 9):
    filename = "../data/runpar-stats/wordleSolver-{}.txt".format(i)
    with open(filename) as f:
      for line in f:
        time = line.split(" ")[0]
        times[i - 1] = min(times[i - 1], float(time))
  print("Fastest time for each CPU: {}".format(times))

if __name__ == "__main__":
  main()