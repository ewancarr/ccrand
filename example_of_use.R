source("randomisation.R")

# Load covariate data frame
covariates <- read.csv("tests/prognostic.csv", header = TRUE)

# Run the script
random_allocation(covariates, 4)
random_allocation(covariates, 6)
random_allocation(covariates, 22)


