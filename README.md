# Advanced R Lab6 Package

[![R](https://img.shields.io/badge/R-package-blue)](https://www.r-project.org/)
[![R-CMD-check](https://github.com/wenli420/lab6knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wenli420/lab6knapsack/actions/workflows/R-CMD-check.yaml)

## Description
The `knapsack` package implements algorithms for solving the classic **0‑1 knapsack problem**.

### Implemented functions
- `brute_force_knapsack()`: Basic brute‑force exhaustive search, returns exact optimal solution with exponential time complexity.
- `brute_force_parallel()`: **Bonus task**. Parallelized brute‑force search, uses multi‑core CPU to reduce runtime of exhaustive enumeration.
- `greedy_knapsack()`: Greedy heuristic algorithm, extremely fast for large datasets, returns an approximate solution.
- `knapsack_dynamic()`: Dynamic programming implementation, exact solution, runtime depends on knapsack maximum weight.
- `knapsack_optimize()`: Additional optimized helper functions for performance tuning.
- Built‑in dataset defined in `data.R` for algorithm testing and benchmarking.

> An experimental Rcpp‑accelerated greedy prototype was developed locally for testing, but excluded from final submission due to cross‑platform compiler compatibility problems.

## Version 0.1.0

Initial release of the `knapsack` package.

## Installation

Install the package directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("wenli420/lab6knapsack")
```

## License

MIT License.
