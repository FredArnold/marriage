# Usage

```haskell
stableMatch example4by4
```

runs the algorithm on the first 4x4 example from [College Admissions and the Stability of Marriage](http://u.arizona.edu/~mwalker/501BReadings/Gale&Shapley_AMM1962.pdf) by Gale and Shapley. It returns:

```haskell
[(1,3),(2,4),(3,1),(4,2)]
```

Each pair represents a marriage. The first element denotes the man, the second denotes the woman, so man 1 marries woman 3.
