library(dplyr)
library(purrr)
library(boot)
library(progressr)

d <- readRDS(file = "data/asd-na_netstats-ran.rds")
orth_poly <- poly(d$nproduced, 3)
d <- bind_cols(d, as_tibble(orth_poly)) %>%
    rename(linear = "1", quadradic = "2", cubic = "3")

models <- list(
    aspl = lm(z_dist ~ (linear + quadradic + cubic) * group, data = d),
    clust = lm(z_clust ~ (linear + quadradic + cubic) * group, data = d),
    indegree = lm(z_indegree_med ~ (linear + quadradic + cubic) * group, data = d)
)
saveRDS(models, "bootstrap/base_models.rds")

newdata <- predict(orth_poly, seq(20, 580, by = 20))
newdata <- tibble(
    group = gl(2, nrow(newdata), labels = levels(d$group)),
    linear = rep(newdata[,1], 2),
    quadradic = rep(newdata[,2], 2),
    cubic = rep(newdata[,3], 2)
)

fbs <- function(df, ix, .data, .formula) {
    df <- df[ix, ]
    m <- lm(.formula, data = df)
    n <- nrow(.data)
    k <- n / 2
    x <- predict(m, .data)
    x <- c(x, x[1:k] - x[(k+1):n])
}

dir.create("bootstrap", showWarnings = FALSE)

# Average shortest path length ----
dir.create(file.path("bootstrap", "aspl"), showWarnings = FALSE)
f <- z_dist ~ (linear + quadradic + cubic) * group
B <- boot(d, fbs, R = 10000, stype = "i", parallel = "multicore", .data = newdata, .formula = f)
saveRDS(B, file = "bootstrap/aspl/boot-10000.rds")


# Clustering Coefficient ----
dir.create(file.path("bootstrap", "clust"), showWarnings = FALSE)
f <- z_clust ~ (linear + quadradic + cubic) * group
B <- boot(d, fbs, R = 10000, stype = "i", parallel = "multicore", .data = newdata, .formula = f)
saveRDS(B, file = "bootstrap/clust/boot-10000.rds")


# Indegree ----
dir.create(file.path("bootstrap", "indegree"), showWarnings = FALSE)
f <- z_indegree_med ~ (linear + quadradic + cubic) * group
B <- boot(d, fbs, R = 10000, stype = "i", parallel = "multicore", .data = newdata, .formula = f)
saveRDS(B, file = "bootstrap/indegree/boot-10000.rds")

