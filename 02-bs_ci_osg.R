#!/usr/bin/env Rscript --vanilla --default-packages=methods,utils,stats,graphics

args <- commandArgs(trailingOnly = TRUE)

metric <- args[1]
value_type <- as.integer(args[2])
bonferroni <- as.logical(as.integer(args[3]))

str(list(metric, value_type, bonferroni))

library(boot)

nsteps <- 29
N <- nsteps * 3
alpha <- .05
conf <- "95"
ix <- (1:nsteps) + (nsteps * value_type)
if (bonferroni == TRUE) {
    alpha <- alpha / nsteps
    conf <- "bonf"
}
outfile <- sprintf(
    "boot-10000_bs-bca_conf-%s_value-%d_metric-%s.rds",
    conf,
    value_type,
    metric
)

# Average shortest path length ----
B <- readRDS("boot-10000.rds")
b_ci <- with_progress({
    p <- progressor(nsteps)
    lapply(ix, function(i, b, conf) {
        p()
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
    }, b = B, conf = 1 - alpha)
})
saveRDS(b_ci, file = outfile)

