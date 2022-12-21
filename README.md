- [Parallel Wordle Solver](#parallel-wordle-solver)
    - [Full Documentation](#full-documentation)
    - [Build](#build)
    - [Help](#help)
    - [Default Behavior (no args)](#default-behavior-no-args)
    - [Play 1 game at a time (`-w`)](#play-1-game-at-a-time--w)
    - [Customizing Heuristics (`-m`)](#customizing-heuristics--m)
    - [Entropy Parallelization Params (`-p`, `-f`, `-e`)](#entropy-parallelization-params--p--f--e)
    - [Interactive Mode (`-i`)](#interactive-mode--i)

# Parallel Wordle Solver

### Full Documentation
Please visit [this pdf](parallel_wordle-report.pdf).

### Build

Make sure to `build` the project before you run any of the commands below:
```bash
ᐅ stack build
```

### Help

```bash
ᐅ stack exec wordleSolver-exe -- -h True +RTS -ls -N5
Usage: wordleSolver [OPTION...]
  -i Bool    --interactive=Bool  Interactive mode
  -m String  --method=String     Heuristic method
  -p String  --parMethod=String  Parallel method
  -f Int     --filterChunk=Int   Filter chunk size
  -e Int     --entropyChunk=Int  Entropy chunk size
  -w String  --word=String       Guess word
  -h         --help              Show help
```

### Default Behavior (no args)

- Initiate 2,309 Wordle games and print out the sequence of turns it took to reach the final answer
- Utilizes the entropy heuristics
- Utilizes the "parMap" parallelization strategy
  - Utilizes `filter_chunk_sz = 50` and `entropy_chunk_sz = 30`
```bash
ᐅ stack exec wordleSolver-exe -- +RTS -ls -N8
"cigar"[3]: ["soare","lucid","cigar"]
"rebut"[3]: ["soare","putid","rebut"]
"sissy"[3]: ["soare","flint","sissy"]
...
```

### Play 1 game at a time (`-w`)

Instead of playing all 2,309 games, we can choose to play only one game by specifying the `-w` option. For example, below is an example of playing one round of Wordle with the answer "baker"
```bash
ᐅ stack exec wordleSolver-exe -- -w "baker" +RTS -ls -N5
["soare","clipt","gombo","deked","baker"]
```

### Customizing Heuristics (`-m`)

While the default heuristics is Entropy, we can choose to use other heuristics. Here are three ways of solving the `prove` wordle game.
```bash
ᐅ stack exec wordleSolver-exe -- -m "frequency" -w "prove" +RTS -ls -N5
["arose","drone","trope","probe","prove"]
ᐅ stack exec wordleSolver-exe -- -m "minimax" -w "prove" +RTS -ls -N5  
["raise","drone","probe","prove"]
ᐅ stack exec wordleSolver-exe -- -m "entropy" -w "prove" +RTS -ls -N5  
["soare","pwned","prove"]
```

### Entropy Parallelization Params (`-p`, `-f`, `-e`)

We can further customize the Entropy Heuristics for it to use different parallelization strategies:
- `-p` specifies the parallelization strategy. Users can choose between `parList` and `parListChunk`
- `-f` specifies the filter chunk size. It's an integer.
- `-e` specifies the entropy chunk size. It's an integer.
Recall that by default, if a user specifies none of the above, we automatically execute `-p parList -f 50 -e 30`.
Here is an example of using the `parListChunk` strategy with filter size of 64 and entropy size of 64:
```bash
ᐅ stack exec wordleSolver-exe -- -m "entropy" -p parListChunk -f 50 -e 50 -w "prove" +RTS -ls -N5  
["soare","pwned","prove"]

ᐅ stack exec wordleSolver-exe -- -m "entropy" -p parListChunk -f 50 -e 50  +RTS -ls -N5  
"cigar"[3]: ["soare","lucid","cigar"]
"rebut"[3]: ["soare","putid","rebut"]
"sissy"[3]: ["soare","flint","sissy"]
...
```

### Interactive Mode (`-i`)

```bash
ᐅ stack exec wordleSolver-exe -- -i True +RTS -ls -N5
Please enter "soare" into NYT's Wordle game.
Enter NYT's response in 0/1/2 format:
...
```