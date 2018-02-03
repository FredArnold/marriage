# Usage

```haskell
stableMatch galeShapleyexample
```

runs the algorithm on the 3x3 example from [College Admissions and the Stability of Marriage](http://u.arizona.edu/~mwalker/501BReadings/Gale&Shapley_AMM1962.pdf) by Gale and Shapley. It returns:

```haskell
[(1,1),(2,2),(3,3)]
```

Each pair represents a marriage. The first element denotes the man, the second denotes the woman.
