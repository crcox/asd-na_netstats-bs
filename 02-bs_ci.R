library(dplyr)
library(purrr)
library(boot)
library(progressr)

nsteps <- 29
N <- nsteps * 3

# Average shortest path length ----
B <- readRDS("bootstrap/aspl/boot-10000.rds")
b_ci <- with_progress({
    p <- progressor(N)
    map(1:N, function(i, b, conf) {
        p()
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
    }, b = B, conf = .95)
})
saveRDS(b_ci, file = "bootstrap/aspl/boot-10000_bs-bca_conf-95.rds")

b_ci <- with_progress({
    p <- progressor(N)
    map(1:N, function(i, b, conf) {
        p()
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
    }, b = B, conf = 1-(0.05 / 29))
})
saveRDS(b_ci, file = "bootstrap/aspl/boot-10000_bs-bca_conf-bonf.rds")


# Clustering coefficient ----
B <- readRDS("bootstrap/clust/boot-10000.rds")
b_ci <- with_progress({
    p <- progressor(N)
    map(1:N, function(i, b, conf) {
        p()
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
    }, b = B, conf = .95)
})
saveRDS(b_ci, file = "bootstrap/clust/boot-10000_bs-bca_conf-95.rds")

b_ci <- with_progress({
    p <- progressor(N)
    map(1:N, function(i, b, conf) {
        p()
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
    }, b = B, conf = 1-(0.05 / 29))
})
saveRDS(b_ci, file = "bootstrap/clust/boot-10000_bs-bca_conf-bonf.rds")


# Indegree ----
B <- readRDS("bootstrap/median/boot-10000.rds")
b_ci <- with_progress({
    p <- progressor(N)
    map(1:N, function(i, b, conf) {
        p()
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
    }, b = B, conf = .95)
})
saveRDS(b_ci, file = "bootstrap/median/boot-10000_bs-bca_conf-95.rds")

b_ci <- with_progress({
    p <- progressor(N)
    map(1:N, function(i, b, conf) {
        p()
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
    }, b = B, conf = 1-(0.05 / 29))
})
saveRDS(b_ci, file = "bootstrap/median/boot-10000_bs-bca_conf-bonf.rds")

