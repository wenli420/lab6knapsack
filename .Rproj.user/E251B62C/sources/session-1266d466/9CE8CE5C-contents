# Tests for knapsack_dynamic(), following the existing knapsack test files.
# Load knapsack_dynamic() before running this file (or run it as a package test).

suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(dk <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500))
  expect_named(dk, c("value", "elements"))
  expect_type(dk$value, "double")
  expect_true(is.numeric(dk$elements))
})

test_that("Function rejects erroneous input", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8, ], W = -3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8, ], W = 0))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8, ], W = "3500"))
  expect_error(knapsack_dynamic(x = data.frame(w = 1:3), W = 10))
  expect_error(knapsack_dynamic(x = data.frame(w = c(1, -2), v = c(3, 4)), W = 10))
  expect_error(knapsack_dynamic(x = data.frame(w = c(1, 2), v = c(3, 0)), W = 10))
})

test_that("Function returns correct optimal results", {
  dk <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_setequal(dk$elements, c(5, 8))

  dk <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_setequal(dk$elements, c(5, 8))

  dk <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_setequal(dk$elements, c(3, 8))

  dk <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_setequal(dk$elements, c(3, 8))
})

test_that("Selected items respect capacity and reported value", {
  for (capacity in c(2000, 3500)) {
    result <- knapsack_dynamic(x = knapsack_objects[1:16, ], W = capacity)
    selected <- result$elements
    expect_true(all(selected %in% seq_len(16)))
    expect_equal(anyDuplicated(selected), 0L)
    expect_lte(sum(knapsack_objects$w[selected]), capacity)
    expect_equal(sum(knapsack_objects$v[selected]), result$value, tolerance = 1e-8)
  }
})

test_that("Simple examples and no fitting objects work", {
  x <- data.frame(w = c(2, 3, 4), v = c(3, 4, 5))
  dk <- knapsack_dynamic(x = x, W = 5)
  expect_equal(dk$value, 7)
  expect_setequal(dk$elements, c(1, 2))

  dk <- knapsack_dynamic(x = x, W = 1)
  expect_equal(dk$value, 0)
  expect_length(dk$elements, 0)
})

test_that("Function completes a medium-sized problem", {
  elapsed <- system.time(
    dk <- knapsack_dynamic(x = knapsack_objects[1:16, ], W = 2000)
  )
  expect_true(is.finite(unname(elapsed["elapsed"])))
  expect_true(is.finite(dk$value))
})
