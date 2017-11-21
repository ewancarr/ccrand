library(here)
source(here("ESM", "rcode", "randomisation.R"))
source(here("ESM", "rcode", "tests", "randomisation_original_adapted.R"))

covariates <- read.csv(here("ESM", "rcode", "tests", "prognostic.csv"))
Z <- read.csv(here("ESM", "block_one_allocation.csv"))

# Old code
random.allocation(covariates, unit = 6)

# New code
random_allocation(covariates, clusters = 6)

additional_allocation(covariates, Z)

