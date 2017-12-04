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
# =============================================================================

determine_clusters <- function(arms, clusters) {
    # This determines the number of clusters, correctly handling an uneven split

    # First, check that arguments are whole numbers
    stopifnot(arms %% 1 == 0,
              clusters %% 1 == 0)

    # If so, calculate number of arms
    return(ifelse(clusters %% arms == 0,        # If number of clusters is 
                                                # divisible by number of arms
                  clusters / arms,              
                  clusters / arms + sample(c(0.5, -0.5), 1)))
}

create_letter_matrix <- function(arms, clusters, per_cluster, half = TRUE) {
    # stopifnot(clusters <= 24)
    # # Create matrix with letters (one for each cluster) arranged into columns
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

create_binary_matrix <- function(arms, clusters, per_cluster, half = TRUE) {
    # Create matrix of all permutations
    all_perm <- expand.grid(rep(list(0:1), clusters)) 
    colnames(all_perm) <- letters[1:clusters]

    # Select rows that sum to number of clusters per arm
    selected_rows <- all_perm[rowSums(all_perm) == per_cluster,]

    if (half) {
        # Return half the matrix
        return(selected_rows[selected_rows[,1] == 1,])
    } else {
        return(selected_rows)
    }
}

random_allocation <- function(covariates, clusters, pick_one = TRUE) {
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

    # Remove cluster IDs ==========================================================
    covariates <- covariates[, -1]

    # Calculate the standardized Z-scores =========================================
    z <- data.frame(scale(covariates, center = TRUE, scale = TRUE))

    # Calculate the balance statistic =============================================
    balance <- cbind(rand, apply((as.matrix(rand) %*% as.matrix(z))**2, 1, sum))
    colnames(balance) <- c(rownames(covariates), "balance")

    #  Sort the data ==============================================================
    balance <- balance[order(balance[,"balance"]), ]

    # Decide how many rows to return ==============================================

    # max_n <- ifelse(clusters >  17, 1000,
    #          ifelse(clusters >  11, 100,
    #          ifelse(clusters == 11, 58,
    #          ifelse(clusters == 10, 32,
    #          ifelse(clusters == 9,  18,
    #          ifelse(clusters <= 8,  nrow(balance)))))))

    # New method: return 1st quartile of nrow(balance)
    max_n <- round(quantile(1:nrow(balance), c(0.10)))
    best_imbalance <- balance[1:max_n, ]

    # Pick a single allocation and random
    selection <- sample(1:max_n, 1)

    # Return single allocation (default) or entire table if requested
    if (pick_one) {
        return(best_imbalance[selection, ])
    } else {
        return(best_imbalance)
    }
}


additional_allocation <- function(covariates, Z, clusters) {
    arms <- 2 
    
    block_size <- ncol(Z) - 1

    # CHECK: What is this doing? ===============================================
    one <- sum(data.frame(Z)[1, 1:ncol(data.frame(Z)) - 1])
    zero <-	ncol(data.frame(Z)) - 1 - one
    # =========================================================================

    # Create required matrices ================================================
    per_cluster <- determine_clusters(arms, clusters)

    random <- create_letter_matrix(arms,
                                   clusters,
                                   per_cluster,
                                   half = FALSE) 

    rand <- create_binary_matrix(arms,
                                 clusters,
                                 per_cluster,
                                 half = FALSE) 

    # Prepare covariates ======================================================
    
    # Check that first column of covariates is string [NOT NECCESSARY?]
    stopifnot(is.character(covariates[,1]) | is.factor(covariates[,1]))

    # Check that we have enough covariates
    stopifnot(nrow(covariates) >= arms * block_size)

    # Set rownames
    rownames(covariates) <- as.character(covariates[,1])

    # Select required rows/columns
    rows <- 1:(clusters + block_size)
    columns <- 2:ncol(covariates)
    selected_covariates <- data.frame(covariates[rows, columns])

    # Calculate the standardized Z-scores
    zscores <- data.frame(scale(selected_covariates,
                                center = TRUE, 
                                scale = TRUE))

    # Select required columns from previous allocation
    Z <- Z[,1:block_size]

    # Set column names for randomisation matrix
    from <- block_size + 1
    to <- block_size + clusters
    colnames(rand) <- rownames(covariates)[from:to]

    # Generates the conditional allocation matrix =============================
    first  	<- 	as.matrix(Z) %*% as.matrix(zscores[1:ncol(Z),]) 

    from <- ncol(Z) + 1
    to <- ncol(Z) + clusters
    second <- as.matrix(rand) %*% as.matrix(zscores[from:to, ])

    first_repeated <- matrix(rep(first, each = nrow(rand)), ncol = ncol(first))

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

    # Return
    return(result)
}
