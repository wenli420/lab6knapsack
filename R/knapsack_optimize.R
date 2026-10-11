# library(profvis)
# 1 individual test brute_force_knapsack,greedy_knapsack,knapsack_dynamic profile from Memory and time
# profvis({
#   brute_force_knapsack(x = knapsack_objects[1:20, ], W = 3500)
# })
#
#
# profvis({
#   for (i in 1:50) {
#     greedy_knapsack(x = knapsack_objects[1:2000, ], W = 2000)
#   }
# })
#
#
# profvis({
#   knapsack_dynamic(x = knapsack_objects[1:1000, ], W = 1000)
# })


# 2 according test time, we find knapsack_dynamic for loops is waste time
# therefore change vector to solve and imporve times
#' Optimized Dynamic Programming Knapsack
#'
#' Solve the knapsack problem using an optimized dynamic
#' programming algorithm.
#'
#' @param x A data frame containing weights (w) and values (v).
#' @param W Maximum weight capacity.
#' @return The result of the optimized knapsack algorithm.
#' @export
knapsack_dynamic_opt <- function(x, W){
  # 2.1 input validation
  if (!is.data.frame(x)){
    stop("x isn't data.frame")
  }
  if (!all(c("w","v") %in% colnames(x))){
    stop("x must contain columns 'w' and 'v'")
  }
  if (!is.numeric(x$w) || !is.numeric(x$v)){
    stop("w and v must be numeric")
  }
  if (!is.numeric(W) || length(W) != 1 || W <= 0) {
    stop("W must be a single positive number")
  }
  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All values in x must be positive")
  }


  # 2.2 initialize DP table
  n <- nrow(x)
  W <- as.integer(W)

  dp <- matrix(0, nrow = n+1, ncol = W+1)

  # 2.3 fill DP table
  for (i in seq_len(n)) {
    wi <- x$w[i]
    vi <- x$v[i]
    # 2.4 Vectorize over all j, removing the inner for(j) loop
    j_all <- 0:W
    not_take <- dp[i, j_all + 1]

    take <- numeric(W+1)
    mask <- j_all >= wi
    take[mask] <- vi + dp[i, j_all[mask] - wi + 1]
    take[!mask] <- not_take[!mask]

    dp[i+1, ] <- pmax(not_take, take)
  }


  # 2.5 backtrack selected items
  best_selected <- integer(0)
  remaining_W <- W
  # 2.6 from the first different to backward find parameters
  for (i in rev(seq_len(n))) {
    if (dp[i + 1, remaining_W + 1] != dp[i, remaining_W + 1]) {
      best_selected <- c(i, best_selected)
      remaining_W <- remaining_W - x$w[i]
    }
  }

  # 2.7 return result
  return(list(
    value = dp[n + 1, W + 1],
    elements = best_selected
  ))

}

# 3 benchmark compare results
# bench::mark(
#   original  = knapsack_dynamic(x = knapsack_objects[1:1200, ], W = 2000),
#   optimized = knapsack_dynamic_opt(x = knapsack_objects[1:1200, ], W = 2000),
#   check = FALSE,
#   iterations = 20
# )

## result table: A tibble: 2 × 13
# expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>
#  1 original      10.1s    10.6s    0.0936    18.4MB     8.43    20  1802      3.56m <NULL> <Rprofmem>
#  2 optimized   194.5ms  367.5ms    2.94     231.2MB    18.8     20   128       6.8s <NULL> <Rprofmem>


