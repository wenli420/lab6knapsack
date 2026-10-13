#' Brute Force Knapsack with Parallel Computing
#'
#' @param x A data.frame containing w and v
#' @param W Maximum knapsack weight
#' @param parallel Logical, if TRUE run in parallel across
#'   detected cores. Default FALSE.
#' @return A list with maximum value and selected elements
#' @export
brute_force_parallel <- function(x, W, parallel = FALSE) {

  # 1. Create the brute_force_parallel function

  # 1.2 Input validation

  # 1.2.1 Check whether x is a data frame
  if (!is.data.frame(x)) {
    stop("x isn't data.frame")
  }

  # 1.2.2 Check whether x contains both required columns
  if (!all(c("w", "v") %in% colnames(x))) {
    stop("x must contain columns 'w' and 'v'")
  }

  # 1.2.3 Check whether weights and values are numeric
  if (!is.numeric(x$w) || !is.numeric(x$v)) {
    stop("w and v must be numeric")
  }

  # 1.2.4 Check whether W is a single positive number
  if (!is.numeric(W) || length(W) != 1 ||
      is.na(W) || !is.finite(W) || W <= 0) {
    stop("W must be a single positive finite number")
  }

  # 1.2.5 Check whether all weights and values are positive
  if (any(!is.finite(x$w)) || any(!is.finite(x$v))) {
    stop("All weights and values must be finite")
  }

  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All values in x must be positive")
  }

  # 1.2.6 Check whether parallel is a single logical value
  if (!is.logical(parallel) ||
      length(parallel) != 1 ||
      is.na(parallel)) {
    stop("parallel must be TRUE or FALSE")
  }

  # 1.3 Determine the number of items
  # n represents the total number of available items
  n <- nrow(x)

  # 1.3.1 Handle empty data frames
  if (n == 0) {
    return(list(
      value = 0,
      elements = integer(0)
    ))
  }

  # 1.4 Warning for large problem sizes
  if (n >= 30) {
    warning(
      "Brute force is O(2^n); n = ", n,
      " combinations will be far too slow to enumerate."
    )
  }

  # 1.4.1 Prevent unsupported problem sizes
  if (n > 30) {
    stop("n must not exceed 30 for this implementation")
  }

  # 2. Serial brute-force implementation

  if (!parallel) {

    # 2.1 Initialize the best solution
    best_value <- 0
    best_elements <- integer(0)

    # 2.2 Enumerate all possible subsets
    for (i in 0:(2^n - 1)) {

      # 2.2.1 Convert the subset index into binary representation
      # 1 means selected and 0 means not selected
      bits <- as.integer(intToBits(as.integer(i)))[seq_len(n)]

      # 2.2.2 Identify the indices of selected items
      selected <- which(bits == 1)

      # 2.2.3 Calculate the total weight of selected items
      total_weight <- sum(x$w[selected])

      # 2.2.4 Calculate the total value of selected items
      total_value <- sum(x$v[selected])

      # 2.2.5 Check whether the subset satisfies
      # the capacity constraint
      if (total_weight <= W && total_value > best_value) {
        best_value <- total_value
        best_elements <- selected
      }
    }

    # 2.3 Return the optimal solution from serial execution
    # Return maximum value and selected item indices
    return(list(
      value = best_value,
      elements = best_elements
    ))
  }

  # 3. Parallel brute-force implementation
  # Execute this section when parallel = TRUE
  # Use mclapply to distribute subset evaluation across CPU cores
  # This implementation is intended for macOS and Linux

  # 3.1 Detect the number of available CPU cores
  n_cores <- parallel::detectCores()

  # 3.1.1 Handle unavailable core information
  if (is.na(n_cores) || n_cores < 1) {
    n_cores <- 1L
  }

  # 3.1.2 Limit the number of workers to available tasks
  n_cores <- min(n_cores, 2L,2^n)

  # 3.2 Generate all possible subset indices
  all_masks <- 0:(2^n - 1)

  # 3.3 Divide all subset indices into separate task chunks
  mask_chunks <- split(
    all_masks,
    cut(seq_along(all_masks), n_cores, labels = FALSE)
  )

  # 3.4 Evaluate the task chunks in parallel
  # Create a cluster
  cl <- parallel::makeCluster(n_cores)

  # Ensure the cluster is stopped when the function exits
  on.exit(parallel::stopCluster(cl), add = TRUE)

  chunk_results <- parallel::parLapply(
    cl,
    mask_chunks,
    function(mask_block, x, W, n) {

      # 3.4.1 Initialize the local best solution for each worker
      local_best_val <- 0
      local_best_elem <- integer(0)

      # 3.4.2 Iterate through all subsets assigned to the worker
      for (i in mask_block) {

        # 3.4.3 Convert each subset index into binary representation
        bits <- as.integer(intToBits(as.integer(i)))[seq_len(n)]

        # 3.4.4 Identify the selected items in the current subset
        selected <- which(bits == 1)

        # 3.4.5 Calculate the total weight of the current subset
        total_weight <- sum(x$w[selected])

        # 3.4.6 Calculate the total value of the current subset
        total_value <- sum(x$v[selected])

        # 3.4.7 Compare the current subset with
        # the local best solution
        # Only feasible subsets with higher values are retained
        if (total_weight <= W &&
            total_value > local_best_val) {
          local_best_val <- total_value
          local_best_elem <- selected
        }
      }

      # 3.4.8 Return the local optimal solution from each worker
      # The main process collects these results for final comparison
      return(list(
        val = local_best_val,
        elem = local_best_elem
      ))

    },
    x,
    W,
    n
  )
  # 3.5 Combine the results obtained from all workers
  # Compare the local optimal solutions to find the global optimum

  # 3.5.1 Initialize the global best solution
  global_best_val <- 0
  global_best_elem <- integer(0)

  # 3.5.2 Iterate through the local results from each worker
  for (res in chunk_results) {

    # 3.5.3 Update the global solution when a better value is found
    if (res$val > global_best_val) {
      global_best_val <- res$val
      global_best_elem <- res$elem
    }
  }

  # 3.6 Return the global optimal solution
  # Return maximum value and selected item indices
  return(list(
    value = global_best_val,
    elements = global_best_elem
  ))
}
