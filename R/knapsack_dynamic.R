#' Dynamic Programming Knapsack
#'
#' @param x A data.frame containing w and v
#' @param W Maximum knapsack weight
#' @return A list with maximum value and selected elements
#' @export
knapsack_dynamic <- function(x, W){
  # 1 input validation
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


  # 2 initialize DP table
  n <- nrow(x)
  W <- as.integer(W)

  dp <- matrix(0, nrow = n+1, ncol = W+1)

  # 3 fill DP table
  for (i in seq_len(n)){
    for(j in 0:W){
      # 3.1 Do not take item i
      not_take <- dp[i, j+1]
      if (j >= x$w[i]){
        # 3.2 take item i
        take <- x$v[i] + dp[i, j - x$w[i] + 1]
        # 3.3 choose maximum value
        dp[i+1,j+1] <- max(not_take,take)
      } else {
        # 3.4 item is too heavy
        dp[i+1,j+1] <- not_take
      }
    }

  }
  # 4 backtrack selected items
  best_selected <- integer(0)
  remaining_W <- W
  # from the first different to backward find parameters
  for (i in seq.int(n, 1)) {
    if (dp[i + 1, remaining_W + 1] != dp[i, remaining_W + 1]) {
      best_selected <- c(i, best_selected)
      remaining_W <- remaining_W - x$w[i]
    }
  }

  # 5 return result
  return(list(
    value = dp[n + 1, W + 1],
    elements = best_selected
  ))

}

