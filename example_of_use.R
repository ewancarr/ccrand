library(here)
source("randomisation.R")
set.seed(11412)

# Initial allocation ===========================================================

# Load covariate data frame 
covariates <- read.csv("tests/prognostic.csv", header = TRUE)

# Run the script
random_allocation(covariates, 4)
random_allocation(covariates, 10, pick_one = FALSE)


# For additional allocations ===================================================

# Load previous allocation
previous_allocation <- read.csv("tests/block_one_allocation.csv")

# Run the script
additional_allocation(covariates, 
                      previous_allocation, 
                      6)

