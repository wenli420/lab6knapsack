#' Brute Force Knapsack with Parallel Computing
#'
#' @param x A data.frame containing w and v
#' @param W Maximum knapsack weight
#' @param parallel Logical, if TRUE run in parallel across
#'   detected cores. Default FALSE.
#' @return A list with maximum value and selected elements
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {

  # 1. Create the brute_force_knapsack function

  # 1.1 Input validation

  # 1.1.1 Check whether x is a data frame
  if (!is.data.frame(x)) {
    stop("x isn't data.frame")
  }

  # 1.1.2 Check whether x contains w and v
  if (!all(c("w", "v") %in% colnames(x))) {
    stop("x must contain columns 'w' and 'v'")
  }

  # 1.1.3 Check whether weights and values are numeric
  if (!is.numeric(x$w) || !is.numeric(x$v)) {
    stop("w and v must be numeric")
  }

  # 1.1.4 Check whether W is a single positive number
  if (!is.numeric(W) || length(W) != 1 ||
      is.na(W) || !is.finite(W) || W <= 0) {
    stop("W must be a single positive number")
  }

  # 1.1.5 Check whether all weights and values are positive
  if (any(!is.finite(x$w)) || any(!is.finite(x$v))) {
    stop("All weights and values must be finite")
  }

  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All values in x must be positive")
  }

  # 1.1.6 Check whether parallel is TRUE or FALSE
  if (!is.logical(parallel) ||
      length(parallel) != 1 ||
      is.na(parallel)) {
    stop("parallel must be TRUE or FALSE")
  }

  # 1.2 Determine the number of items
  n <- nrow(x)

  # 1.2.1 Handle empty data frames
  if (n == 0) {
    return(list(
      value = 0,
      elements = integer(0)
    ))
  }

  # 1.3 Warning for large problem sizes
  if (n >= 30) {
    warning(
      "Brute force is O(2^n); n = ", n,
      " combinations will be far too slow to enumerate."
    )
  }

  # 1.3.1 Prevent unsupported problem sizes
  if (n > 30) {
    stop("n must not exceed 30 for this implementation")
  }

  # 2. Serial brute-force implementation

  # 2.1 Execute serial computation when parallel = FALSE
  if (!parallel) {

    # 2.1.1 Initialize the best solution
    best_value <- 0
    best_elements <- integer(0)

    # 2.1.2 Iterate over all possible subsets
    for (i in 0:(2^n - 1)) {

      # 2.1.3 Convert integer to binary representation
      bits <- as.integer(
        intToBits(as.integer(i))
      )[seq_len(n)]

      # 2.1.4 Get selected items
      selected <- which(bits == 1)

      # 2.1.5 Calculate total weight and value
      total_weight <- sum(x$w[selected])
      total_value <- sum(x$v[selected])

      # 2.1.6 Update the best solution
      if (total_weight <= W &&
          total_value > best_value) {
        best_value <- total_value
        best_elements <- selected
      }
    }

    # 2.2 Return the serial result
    return(list(
      value = round(best_value),
      elements = best_elements
    ))
  }

  # 3. Parallel brute-force implementation

  # 3.1 Execute parallel computation when parallel = TRUE

  # 3.1.1 Detect the number of available CPU cores
  n_cores <- parallel::detectCores()

  # 3.1.2 Handle unavailable core information
  if (is.na(n_cores) || n_cores < 1) {
    n_cores <- 1L
  }

  # 3.1.3 Limit the number of workers
  n_cores <- min(n_cores, 2L, 2^n)

  # 3.2 Divide all possible subsets into chunks

  # 3.2.1 Calculate the total number of subsets
  total_combinations <- 2^n

  # 3.2.2 Calculate the boundaries for each worker
  boundaries <- floor(
    seq(
      0,
      total_combinations,
      length.out = n_cores + 1
    )
  )

  # 3.2.3 Divide the subset indices into chunks
  mask_chunks <- lapply(
    seq_len(n_cores),
    function(i) {
      c(boundaries[i], boundaries[i + 1] - 1)
    }
  )

  # 3.3 Create a parallel cluster

  # 3.3.1 Create workers
  cl <- parallel::makeCluster(n_cores)

  # 3.3.2 Stop the cluster when the function exits
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # 3.4 Evaluate the chunks in parallel
  chunk_results <- parallel::parLapply(
    cl,
    mask_chunks,
    function(mask_block, items, W, n) {

      # 3.4.1 Initialize the local best solution
      local_best_val <- 0
      local_best_elem <- integer(0)

      # 3.4.2 Iterate through subsets assigned to worker
      for (i in seq.int(mask_block[1], mask_block[2])) {

        # 3.4.3 Convert integer to binary representation
        bits <- as.integer(
          intToBits(as.integer(i))
        )[seq_len(n)]

        # 3.4.4 Get selected items
        selected <- which(bits == 1)

        # 3.4.5 Calculate total weight and value
        total_weight <- sum(items$w[selected])
        total_value <- sum(items$v[selected])

        # 3.4.6 Update the local best solution
        if (total_weight <= W &&
            total_value > local_best_val) {
          local_best_val <- total_value
          local_best_elem <- selected
        }
      }

      # 3.4.7 Return the local best solution
      return(list(
        val = local_best_val,
        elem = local_best_elem
      ))
    },
    items = x,
    W = W,
    n = n
  )

  # 3.5 Combine the results from all workers

  # 3.5.1 Initialize the global best solution
  global_best_val <- 0
  global_best_elem <- integer(0)

  # 3.5.2 Compare local results from each worker
  for (res in chunk_results) {

    # 3.5.3 Update the global best solution
    if (res$val > global_best_val) {
      global_best_val <- res$val
      global_best_elem <- res$elem
    }
  }

  # 3.6 Return the global optimal solution
  return(list(
    value = round(global_best_val),
    elements = global_best_elem
  ))
}
