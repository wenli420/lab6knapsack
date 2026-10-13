#' greedy_knapsack
#'
#' @param x is data.frame containing w and v
#' @param W is maximum knapsack weight
#' @return A list with maximum value and selected elements
#' @export

greedy_knapsack <- function(x, W){
  # 1 create the greedy function
  # 1.1 judge the parameter
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

  # 2. calculate the ratio of per item
  ratio <- x$v / x$w
  order_idx <- order(ratio, decreasing = TRUE)
  # w_sorted <- x$w[ra_order]
  # v_sorted <- x$v[ra_order]

  # 3. summary the total value and weight
  # 3.1 initial
  total_weight <- 0
  total_value <- 0
  # Pre-allocate memory to improve performance
  best_selects <- integer(nrow(x))
  count <- 0L
  # 3.2 calculate until sum_w > W
  for (i in order_idx){
    if (total_weight+x$w[i] <= W){
       total_weight<- total_weight+x$w[i]
       total_value<-total_value+x$v[i]
       count <- count + 1L
       best_selects[count] <- i
    }else{
      # if don't setting which add the next one
      break
    }
  }
  # 4. return the parameter
  return(list(
    value = round(total_value),
    elements = best_selects[seq_len(count)]
  ))

}


#greedy_knapsack(
#  x = knapsack_objects[1:1200, ],
#  W = 2000
#)
