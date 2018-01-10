library(here)
source("randomisation.R")
set.seed(11412)

# Initial allocation ===========================================================

# Load covariate data frame 
covariates <- read.csv("tests/prognostic.csv", header = TRUE)

# Run the script
random_allocation(covariates, 4, pick_one = FALSE)
random_allocation(covariates, 4, pick_one = TRUE)

random_allocation(covariates, 10, pick_one = FALSE)
random_allocation(covariates, 10, pick_one = TRUE)

# For additional allocations ===================================================

# Load previous allocation
previous_allocation <- read.csv("tests/block_one_allocation.csv")
Z <- previous_allocation[, c(1:6, ncol(previous_allocation))]

# Run the script
test <- additional_allocation(covariates, Z, clusters = 6)

