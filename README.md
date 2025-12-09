# My puzzle solutions

### For [Project Euler](https://projecteuler.net/archives) and [Advent of Code](https://adventofcode.com/)

[![Profile image](https://projecteuler.net/profile/gumgl.png)](https://projecteuler.net/progress=gumgl)

## Computing solutions

### Advent of Code

Run from the root directory:

```console
./run.ps1 YYYY DD
```

Use example input with `-e`.

### Project Euler

TODO: add runners

## Adding solutions

### In Haskell:

1. Create your file `Solution.hs` in `/aoc/Y{year}/D{day}/` or `/euler/P{n}/`
   1. with the appropriate module name at the top e.g. `module Aoc.Y2024.D03.Solution where`
   1. implement the solve function:
      ```haskell
      solve :: String -> (String, String)
      solve content = (part1 content, part2 content)
      ```
1. Add in the [cabal project file](puzzles.cabal), in the relevant section under `other-modules:`, your module name
1. Add to the relevant runner (e.g. [`/aoc/Runner.hs`](/aoc/Runner.hs)) by adding both the `import` and a mapping in the definition of `solve`

### In Python:

1. Simply create your file as `/aoc/Y{year}/D{day}/solution.py` or `/euler/P{n}/Solution.py`