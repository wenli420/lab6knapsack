#' Brute Force Knapsack
#'
#' @param x A data.frame containing w and v
#' @param W Maximum knapsack weight
#' @return A list with maximum value and selected elements
#' @importFrom stats runif
#' @export
brute_force_knapsack <- function(x,W){
  # 1. create function brute_force_knapsack
  # 1.1 x= data_frame [1:n] items - 1:n, W is limit weight
  # 1.2 input validation
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

  # 1.3 number of objects
  n <- nrow(x)

  # 1.4 warning for large n
  if (n >= 30){
    warning("Brute force is O(2^n); n = ", n,
            " combinations will be far too slow to enumerate.")
  }

  # 1.5 initialize best solution
  best_value <- 0
  best_elements <- integer(0)

  # 1.6 iterate over all combinations
  for (i in 0:(2^n-1)){
    # convert integer to binary representation
    bits <- as.integer(intToBits(i))[seq_len(n)]
    # get selected items
    selected <- which(bits == 1)
    # calculate total weight and value
    total_weight <- sum(x$w[selected])
    total_value <- sum(x$v[selected])
    # update best solution
    if (total_weight <= W && total_value > best_value) {
      best_value <- total_value
      best_elements <- selected
    }
  }
  # 1.7 return the result
  return(list(
    value = round(best_value),
    elements = best_elements
  ))
}

# test the function
#result1 <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#result1
