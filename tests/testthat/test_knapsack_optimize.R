# Tests for knapsack_dynamic_opt (optimized dynamic‑programming knapsack)

test_that("knapsack_dynamic_opt matches results from original knapsack_dynamic", {
  data(knapsack_objects)

  dk_old <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
  dk_opt <- knapsack_dynamic_opt(x = knapsack_objects[1:8, ], W = 3500)
  expect_equal(round(dk_opt$value), round(dk_old$value))
  expect_setequal(dk_opt$elements, dk_old$elements)

  dk_old2 <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 3500)
  dk_opt2 <- knapsack_dynamic_opt(x = knapsack_objects[1:12, ], W = 3500)
  expect_equal(round(dk_opt2$value), round(dk_old2$value))
  expect_setequal(dk_opt2$elements, dk_old2$elements)

  dk_old3 <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 2000)
  dk_opt3 <- knapsack_dynamic_opt(x = knapsack_objects[1:8, ], W = 2000)
  expect_equal(round(dk_opt3$value), round(dk_old3$value))
  expect_setequal(dk_opt3$elements, dk_old3$elements)

  dk_old4 <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 2000)
  dk_opt4 <- knapsack_dynamic_opt(x = knapsack_objects[1:12, ], W = 2000)
  expect_equal(round(dk_opt4$value), round(dk_old4$value))
  expect_setequal(dk_opt4$elements, dk_old4$elements)
})

test_that("knapsack_dynamic_opt correctly validates input", {
  data(knapsack_objects)

  expect_error(knapsack_dynamic_opt("hej", 3500))
  expect_error(knapsack_dynamic_opt(x = knapsack_objects[1:8, ], W = -3500))
  expect_error(knapsack_dynamic_opt(x = knapsack_objects[1:8, ], W = 0))
  expect_error(knapsack_dynamic_opt(x = knapsack_objects[1:8, ], W = "3500"))
  expect_error(knapsack_dynamic_opt(x = data.frame(w = 1:3), W = 10))
  expect_error(knapsack_dynamic_opt(x = data.frame(w = c(1, -2), v = c(3, 4)), W = 10))
  expect_error(knapsack_dynamic_opt(x = data.frame(w = c(1, 2), v = c(3, 0)), W = 10))
})

test_that("knapsack_dynamic_opt works for small hand‑written examples", {
  x <- data.frame(w = c(2, 3, 4), v = c(3, 4, 5))
  dk_old  <- knapsack_dynamic(x = x, W = 5)
  dk_opt  <- knapsack_dynamic_opt(x = x, W = 5)

  expect_equal(dk_opt$value, dk_old$value)
  expect_setequal(dk_opt$elements, dk_old$elements)

  dk_old_none <- knapsack_dynamic(x = x, W = 1)
  dk_opt_none <- knapsack_dynamic_opt(x = x, W = 1)
  expect_equal(dk_opt_none$value, dk_old_none$value)
  expect_length(dk_opt_none$elements, 0)
})

test_that("knapsack_dynamic_opt output satisfies weight constraints", {
  data(knapsack_objects)
  result <- knapsack_dynamic_opt(x = knapsack_objects[1:16, ], W = 2000)
  selected <- result$elements

  expect_true(all(selected %in% seq_len(16)))
  expect_equal(anyDuplicated(selected), 0L)
  expect_lte(sum(knapsack_objects$w[selected]), 2000)
  expect_equal(round(sum(knapsack_objects$v[selected])), round(result$value))
})

test_that("knapsack_dynamic_opt completes medium‑size instance without crash", {
  data(knapsack_objects)
  elapsed <- system.time(
    dk_opt <- knapsack_dynamic_opt(x = knapsack_objects[1:16, ], W = 2000)
  )
  expect_true(is.finite(unname(elapsed["elapsed"])))
  expect_true(is.finite(dk_opt$value))
})
