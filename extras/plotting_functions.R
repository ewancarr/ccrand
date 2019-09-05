library(tidyverse)

#  ┌──────────────────────────────────────────────────┐ 
#  │                                                  │
#  │                Plotting functions                │
#  │                                                  │
#  └──────────────────────────────────────────────────┘

merge_covariates <- function(final_allocation, covariates) {
    names(covariates) <- tolower(names(covariates))
    # The 'covariates' data frame MUST have a column labelled 'cluster'.
    stopifnot("cluster" %in% names(covariates))

    final <- final_allocation %>%
        select(-balance) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        filter(rowname != "balance") %>%
        set_names(c("cluster", "group")) %>%
        mutate(group = factor(group))
    merged <- covariates %>%
        set_names(tolower) %>%
        mutate(cluster = as.character(cluster)) %>%
        full_join(final) %>%
        arrange(group) %>%
        filter(!is.na(group))
    return(merged)
}

check_means <- function(allocation_with_covariates) {
    allocation_with_covariates %>%
    group_by(group) %>%
        summarise_if(is.numeric, mean) %>%
        gather(key, value, -group) %>%
        ggplot(aes(x = group, y = value, fill = key)) +
        geom_col() +
        facet_wrap(~ key, scales = "free") +
        theme(legend.position = "none")
}

check_ordering <- function(allocation_with_covariates) {
    allocation_with_covariates %>%
        gather(key, value, -cluster, -group) %>%
        ungroup() %>%
        arrange(key, value) %>%
        mutate(order = row_number()) %>%
        ggplot(aes(x = order, y = value, fill = group)) +
        geom_col() +
        facet_wrap(~ key, scales = "free")
}
