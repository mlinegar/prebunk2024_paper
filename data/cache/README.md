# Cached Permutation Results

These small CSV files store the 10,000-iteration permutation-test results used by `functions/balance_and_checks_quick.R`.

Regenerate them after changing the raw data or balance-test specification by sourcing the data-processing/setup scripts and then running:

```sh
Rscript functions/balance_and_checks_permutations.R
```

