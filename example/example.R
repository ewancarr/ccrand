library(tidyverse)
source("../randomisation.R")
set.seed(11412)

# Get initial allocation ======================================================

# Load covariate data frame 
covariates <- read.csv("prognostic.csv", header = TRUE)

# Run the script
random_allocation(covariates, 4)
# random_allocation(covariates, 8)
# random_allocation(covariates, 10)

# Get additional allocations ==================================================

# Load previous allocation
allocation <- random_allocation(covariates, 4)

# Run the script
allocation  <- additional_allocation(covariates, allocation, clusters = 6)
