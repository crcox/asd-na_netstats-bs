library(dplyr)
library(purrr)
library(boot)
library(progressr)

# Data from Stan
load("data/ASD_NA_metadataNetstats_2023_RAN_14Aug.Rdata")

# Previous input data frame
d <- readRDS(file = "data/asd-na_netstats-ran.rds")

# Subset, filter to discrete, and compare
e <- ASD_NA_metadataNetstats_2023_RAN %>%
    select(
        subjectkey,
        interview_age,
        sex,
        nproduced = nProduced,
        group,
        z_indegree_med,
        z_clust,
        z_dist
    ) %>%
    ungroup() %>%
    distinct()

str(d)
str(e)

# Save the new base data
saveRDS(e, file = "data/asd-na_netstats-ran_2023Aug14.rds")
