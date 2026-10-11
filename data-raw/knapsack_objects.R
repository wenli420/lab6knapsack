# Generate the package data set `knapsack_objects`

# 1. setting r version and ignore  warning
suppressWarnings(
  RNGversion(min(as.character(getRversion()), "3.5.3"))
)
# 2. set random seed, type = MT, n= 2000，v = 1-10000，and w = 1-4000.
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = base::sample(1:4000, size = n, replace = TRUE),
  v = stats::runif(n = n, 0, 10000)
)
# 3 save the data as .rda
save(knapsack_objects, file = "../data/knapsack_objects.rda", compress = TRUE)

# 4 print finish
cat("save success!\n")
