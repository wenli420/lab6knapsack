# Advanced R Lab6 Package

[![R](https://img.shields.io/badge/R-package-blue)](https://www.r-project.org/)
[![R-CMD-check](https://github.com/wenli420/lab6knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wenli420/lab6knapsack/actions/workflows/R-CMD-check.yaml)

## Description

The `knapsack` package implements algorithms for solving the **0-1 knapsack problem**, including brute-force search, dynamic programming, and a greedy heuristic. The brute-force function supports optional parallel computing.

## Implemented Functions

- `brute_force_knapsack()`: Exact brute-force search with `parallel = FALSE` (serial) or `parallel = TRUE` (parallel).
- `knapsack_dynamic()`: Exact dynamic programming solution.
- `knapsack_dynamic_opt()`: Optimized dynamic programming implementation (in `R/knapsack_optimize.R`).
- `greedy_knapsack()`: Fast greedy heuristic.

The package also includes the `knapsack_objects` dataset.

## Project Structure

```text
lab6knapsack/
├── .github/                  # GitHub Actions
├── R/                        # R functions and data documentation
│   ├── brute_force_knapsack.R
│   ├── data.R
│   ├── greedy_knapsack.R
│   ├── knapsack_dynamic.R
│   └── knapsack_optimize.R
├── data/                     # Package dataset
├── data-raw/                 # Data preparation
├── man/                      # Function documentation
├── tests/
│   └── testthat/             # Unit tests
│       ├── test_brute_force_knapsack.R
│       ├── test_greedy_knapsack.R
│       └── test_knapsack_dynamic.R
├── vignettes/
│   └── introduction.Rmd      # Lab report and benchmarks
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── LICENSE.md
└── README.md
```

## Clone and Install Locally

Clone the repository using SSH (requires your GitHub SSH key to be configured):

```bash
git clone git@github.com:wenli420/lab6knapsack.git
cd lab6knapsack
```

```r
# If needed, install devtools once:
# install.packages("devtools")
devtools::install()
library(knapsack)
```

## Testing

From the package root in RStudio, run:

```r
devtools::test()
devtools::check()
```

See the [introduction vignette](vignettes/introduction.Rmd) for algorithm demonstrations, correctness checks, and parallel benchmark results.

## License

MIT License. See [LICENSE.md](LICENSE.md).
