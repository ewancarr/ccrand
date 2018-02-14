# ==============================================================================
# Title:            This script calculates the balance statistic using the 
#                   minimisation algorithm.
# Author:           Ewan Carr
# Started:          2017-11-17
# Original author:  Ben Carter
# Original date:    2007-12-04
            
# CHANGES
# Date	        Description
# ------------  ---------------------------------------------------------------
# 2008-05-18    Added uneven arm split randomly selected into first block
# 2017-11-21    Rewrote script
#                   - Replaced loop to create 0/1 matrix; can now handle
#                     4+ clusters.
#                   - Various improvements for readability
# 2017-11-28    Added option to return a single allocation, selected at random,
#               set as default.
# 2017-12-12    Revised script
#                   - Now uses all possible allocations (i.e. including 
#                     duplicates -- 0011 AND 1100).
#                   - With 4 clusters, the first allocation will give 6 
#                     possible allocations with 3 unique balance statistic.
#                     The function will return one of the top two allocations
#                     at random (i.e. two allocations with identical balance).
#               Simplified options. Now returns a list with "all_allocations"
#               and the "single_allocation", selected at random.
# =============================================================================

covariate_filename <- function(allo) {
    paste0("covariates_allocation_", allo, ".xlsx")
}

generate_filename_datestamp <- function(allo) {
    paste0("ODDESSI_",
           format(Sys.time(), "%Y_%m_%d_%H%M%S"),
           "_allocation_", allo, ".Rdata")
}

load_existing_allocation <- function(allo) {
    # allo = CURRENT allocation (i.e. 1, 2, 3...)
    # Previous allocations should be stored in a folder 
    # "saved_allocations".

    # Check we're not on the first allocation.
    if (allo < 2) {
        stop("Must be on second allocation (or higher).")
    }

    search_term <- paste0("^.*_allocation_", allo - 1, "\\.Rdata")

    # Check the previous allocation is stored in correct folder.
    if(!any(str_detect(dir(here("saved_allocations")), search_term))) {
        stop("Previous allocation not found.")
    }

    # Check there is only one version of the previous allocation. 
    matched_files <- str_match(dir(here("saved_allocations")), search_term)
    matched_files <- matched_files[!is.na(matched_files)]
    if (length(matched_files) > 1) {
        stop(paste0("I found multiple versions of the previous allocation:\n\n",
                    paste(matched_files, collapse = "\n"),
                    "\n\nPlease resolve."))
    }

    # Load the previous allocation; check with user that correct file is loaded
    ask_user <- menu(c("Yes", "No"),
                     title = paste0("I found this file. ",
                                    "Is this the correct previous allocation?",
                                    "\n\n",
                                    "    ", matched_files,
                                    "\n"))
    if (ask_user == 1) {
        load(here("saved_allocations", matched_files), 
             verbose = TRUE)
    } else {
        stop("Stopping, as requested.")
    }
}



balance_so_far <- function(previous_allocation) {
    pa <- previous_allocation$single_allocation
    split <- table(t(pa[, -ncol(pa)]))
    return(split[[1]] - split[[2]])
}

determine_clusters <- function(arms, clusters) {
    # This determines the number of clusters, correctly handling an 
    # uneven split.

    # Check that arguments are whole numbers
    stopifnot(arms %% 1 == 0,
              clusters %% 1 == 0)

    # Check that we have > 1 cluster
    stopifnot(clusters > 1)

    # If so, calculate number of arms
    return(ifelse(clusters %% arms == 0,        # If number of clusters is 
                                                # divisible by number of arms
                  clusters / arms,              
                  clusters / arms + sample(c(0.5, -0.5), 1)))
}

create_letter_matrix <- function(arms, clusters, per_cluster, half = TRUE) {
    # stopifnot(clusters <= 24)
    # Create matrix with letters (one for each cluster) arranged into columns
    # (one for each arm)
    random <- t(combn(letters[1:clusters], per_cluster))
    if (half) {
        half <- nrow(random) / 2
        random	<-	random[1:half, ]
        return(random)
    } else {
        return(random)
    }
}

create_binary_matrix <- function(arms, clusters, per_cluster) {
    # Create matrix of all permutations
    all_perm <- expand.grid(rep(list(0:1), clusters)) 
    colnames(all_perm) <- letters[1:clusters]

    # Select rows that sum to number of clusters per arm
    selected_rows <- all_perm[rowSums(all_perm) == per_cluster,]

    return(selected_rows)
}

random_allocation <- function(covariates, clusters) {
    # Check that 'covariates' is, or can be converted to, a data frame.
    if (is.tibble(covariates)) {
        covariates <- as.data.frame(covariates)
    }

    # Throw an error if covariates isn't a data frame
    stopifnot(is.data.frame(covariates))

    arms <- 2       # Fixed, for now.

    # Calculate the number of clusters per arm ================================
    per_cluster <- determine_clusters(arms, clusters)

    # Generate matrix of letters ==============================================
    random <- create_letter_matrix(arms, clusters, per_cluster)

    # Generate matrix of 0/1s =================================================
    rand <- create_binary_matrix(arms, clusters, per_cluster)
    
    # Check that we have covariate information for each cluster ===============
    stopifnot(nrow(covariates) >= clusters)

    # Set row names
    rownames(covariates) <- as.character(covariates[,1])

    # Select first n (clusters) for practice
    covariates <- data.frame(covariates[1:clusters, ])

    # Remove cluster IDs ======================================================
    covariates <- covariates[, -1]

    # Calculate the standardized Z-scores =====================================
    z <- data.frame(scale(covariates, center = TRUE, scale = TRUE))

    # Calculate the balance statistic =========================================
    balance <- cbind(rand, apply((as.matrix(rand) %*% as.matrix(z))**2, 1, sum))
    colnames(balance) <- c(rownames(covariates), "balance")

    #  Sort the data ==========================================================
    balance <- balance[order(balance[,"balance"]), ]

    # Decide how many rows to return ==========================================
    n <- round(quantile(1:nrow(balance), c(0.10)))

    if (clusters == 4) {
        # If using 4 clusters (6 possible allocations) pick at random one of the 
        # top two best balanced allocations.
        allocation <- balance[sample(1:2, 1), ]
    } else {
        # Otherwise, pick at random one allocation from the top 10% of possible 
        # allocations (i.e. with best balance).
        allocation <- balance[sample(1:n, 1), ]
    }

    # Return the final allocation
    return(list(single_allocation = allocation,
                all_allocations = balance,
                site_size = clusters))
}

additional_allocation <- function(covariates, 
                                  previous_allocation,
                                  clusters,
                                  fix_balance = FALSE,
                                  verbose = TRUE) {

    # Check that 'covariates' is, or can be converted to, a data frame.
    if (is.tibble(covariates)) {
        covariates <- as.data.frame(covariates)
    }

    # Throw an error if covariates isn't a data frame
    stopifnot(is.data.frame(covariates))
    arms <- 2 
    Z <- previous_allocation$single_allocation
    size_of_existing_allocation <- ncol(Z) - 1

    # Check previous allocation ===============================================

    # Should have a single column labelled "balance"
    stopifnot("balance" %in% tolower(names(Z)))

    # Create required matrices ================================================
    per_cluster <- determine_clusters(arms, clusters)
    if (verbose) {
        cat(paste0("\n", 
                   "Number of clusters per arm:                          ", 
                   per_cluster, "\n"))
    }

    random <- create_letter_matrix(arms, clusters, per_cluster) 
    rand <- create_binary_matrix(arms, clusters, per_cluster)

    # Prepare covariates ======================================================
    if (verbose) {
        cat(paste0("Cluster ID variable:                                 ", 
                   names(covariates)[1]))
        cat(paste0("\n", 
                   "Balancing on the following variables:                ", 
                   paste(names(covariates)[-1], 
                         collapse = ", "),
                   "\n"))
    }

    # Check that we have enough covariates
    if (verbose) {
        cat(paste0("Rows in covariates:                                  ", 
                   nrow(covariates), "\n"))
        cat(paste0("Number of clusters ALREADY allocated:                ", 
                   size_of_existing_allocation, "\n"))
        cat(paste0("Number of clusters in the CURRENT allocation         ", 
                   clusters, "\n"))
    }

    stopifnot(nrow(covariates) >= (size_of_existing_allocation + clusters))

    # Set rownames
    rownames(covariates) <- as.character(covariates[,1])

    # Select required rows/columns of covariate data frame
    rows <- 1:(clusters + size_of_existing_allocation)
    columns <- 2:ncol(covariates)
    selected_covariates <- data.frame(covariates[rows, columns])

    # Calculate the standardized Z-scores
    z_scores <- data.frame(scale(selected_covariates,
                                center = TRUE, 
                                scale = TRUE))

    # Select required columns from previous allocation (i.e. remove 'balance')
    Z <- Z[,1:size_of_existing_allocation]

    # Set column names for randomisation matrix (for new allocation)
    from <- size_of_existing_allocation + 1
    to <- size_of_existing_allocation + clusters
    colnames(rand) <- rownames(covariates)[from:to]

    # Generate the conditional allocation matrix =============================

    # Ensure that existing allocation is numeric (and not integer). This is 
    # required for the matrix multiplication.

    Z <- t(apply(Z, 2, as.numeric))

    # For the existing allocation (repeated down the columns)
    
    first <- as.matrix(Z) %*% as.matrix(z_scores[1:ncol(Z),]) 
    from <- ncol(Z) + 1
    to <- ncol(Z) + clusters
    first_repeated <- matrix(rep(first, each = nrow(rand)), ncol = ncol(first))

    # For the new, potential allocations
    second <- as.matrix(rand) %*% as.matrix(z_scores[from:to, ])

    # Calculate balance statistic for combined allocation (existing, and new
    # potential allocations)
    balance <- rowSums((first_repeated + second) ** 2)

    # Merge with block allocation =============================================
    second_allocat <- cbind(rand, data.frame(balance))

    # Sort by balance =========================================================
    second_allocat <- second_allocat[order(second_allocat[,"balance"]), ]

    # Decide how many rows to return ==========================================
    max_n <- ifelse(nrow(second_allocat) > 1000, 
                    1000, 
                    nrow(second_allocat))

    # Merge in the conditional allocation =====================================
    initial_allocation_repeated <- matrix(rep(Z, each = max_n), ncol = ncol(Z))
    result <- cbind(initial_allocation_repeated, second_allocat[1:max_n,])

    # Set column names
    colnames(result) <- c(rownames(covariates)[1:(ncol(Z) + clusters)],
                          "balance")

    # Derive final allocations ================================================
    allocation <- result[sample(1:round(quantile(1:nrow(second_allocat), 
                                          c(0.1))), 1),]
    allocation <- list(single_allocation = allocation,
                       all_allocations = result,
                       site_size = c(previous_allocation$site_size, clusters))

    # =========================== OPTIONAL ====================================

    # We can optionally ensure that the sites are balanced across arms.

    # If the previous allocation was balanced 50:50, then we make no changes to
    # the allocation result. 
    
    # However, if the CURRENT allocation has become unbalanced, we will UPDATE
    # (i.e. fix) the current allocation to ensure a 50:50 balance is achieved.
    # We will do this by changing one of the clusters (chosen at random) to be
    # '0' or '1', depending on the direction of the existing imbalance.

    # By default this is disabled.

    balanced <- allocation

    if (balance_so_far(balanced) >= 1) {
        if (verbose) { cat("We have too many 1s, so replace one of the 0s.\n") }

        # Pick one 0 column at random
        zeros <- which(balanced$single_allocation == 0)
        chosen_zero <- sample(zeros, 1)
        
        # Replace chosen 0 in single allocation
        balanced$single_allocation[chosen_zero] <- 1

        # Replace chosen 0 in 'all_allocations' table
        balanced$all_allocations[, chosen_zero] <- 1

    } else if (balance_so_far(balanced) <= -1) {
        if (verbose) { cat("We have too many 0s, so replace one of the 1s.\n") }

        # Pick one 1 column at random
        ones <- which(balanced$single_allocation == 1)
        chosen_one <- sample(ones, 1)
        
        # Replace chosen 0 in single allocation
        balanced$single_allocation[chosen_one] <- 0

        # Replace chosen 0 in 'all_allocations' table
        balanced$all_allocations[, chosen_one] <- 0
    }

    if (verbose) {
        cat(paste0("\nArm imbalance across sites for PREVIOUS allocations: ", 
                   balance_so_far(previous_allocation)))
        cat(paste0("\nArm imbalance across sites for new allocation:       ", 
                   balance_so_far(allocation), "\n"))
    }

    # ====================== RETURN FINAL ALLOCATIONS =========================

    if (fix_balance) {
        return(balanced)
    } else {
        return(allocation)
    }
}
