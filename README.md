# Advanced R Lab6 Package

[![R](https://img.shields.io/badge/R-package-blue)](https://www.r-project.org/)
[![R-CMD-check](https://github.com/wenli420/lab6knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wenli420/lab6knapsack/actions/workflows/R-CMD-check.yaml)

## Description

The `knapsack` package implements algorithms for solving the **0-1 knapsack problem**, including brute-force search, dynamic programming, and a greedy heuristic. The brute-force function also supports optional parallel computing.

## Implemented Functions

- `brute_force_knapsack()`: Exact brute-force search, with `parallel = FALSE` or `parallel = TRUE`.
- `knapsack_dynamic()`: Exact dynamic programming solution.
- `greedy_knapsack()`: Fast greedy heuristic.

## Project Structure

```text
lab6knapsack/
├── .github/                 # GitHub Actions workflow
├── R/                       # Algorithm implementations and data definitions
│   ├── brute_force_knapsack.R
│   ├── greedy_knapsack.R
│   ├── knapsack_dynamic.R
│   ├── knapsack_optimize.R
│   └── data.R
├── data/                    # Included dataset
├── data-raw/                # Data preparation
├── man/                     # Function documentation
├── tests/testthat/          # Unit tests
├── vignettes/
│   └── introduction.Rmd     # Detailed report and benchmarks
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── LICENSE.md
└── README.md
```

## Installation

Install from GitHub:

```r
install.packages("remotes")  # Only needed once
remotes::install_github("wenli420/lab6knapsack")
library(knapsack)
```

## Testing

Quickly verify the serial and parallel brute-force functions:

```r
x <- data.frame(w = c(2, 3, 4, 5), v = c(3, 4, 5, 6))

serial <- brute_force_knapsack(x, W = 8, parallel = FALSE)
parallel <- brute_force_knapsack(x, W = 8, parallel = TRUE)

stopifnot(identical(serial, parallel))
```

Run package tests and checks from the package root:

```r
# Install devtools if needed: install.packages("devtools")
devtools::test()
devtools::check()
```

See [the introduction vignette](vignettes/introduction.Rmd) for the full analysis and benchmark results.

## License

MIT License. See [LICENSE.md](LICENSE.md).
