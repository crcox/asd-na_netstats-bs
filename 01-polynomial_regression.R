library(dplyr)
library(purrr)
library(boot)
library(progressr)

d <- readRDS(file = "data/asd-na_netstats-ran.rds")
orth_poly <- poly(d$nproduced, 3)
d <- bind_cols(d, as_tibble(orth_poly)) %>%
    rename(linear = "1", quadradic = "2", cubic = "3")

summary(d)

newdata <- predict(orth_poly, seq(20, 580, by = 20))
newdata <- tibble(
    group = gl(2, nrow(newdata), labels = levels(d$group)),
    linear = rep(newdata[,1], 2),
    quadradic = rep(newdata[,2], 2),
    cubic = rep(newdata[,3], 2)
)

m <- list(
    median = lm(z_indegree_med ~ (linear + quadradic + cubic) * group, data = d),
    clust = lm(z_clust ~ (linear + quadradic + cubic) * group, data = d),
    aspl = lm(z_dist ~ (linear + quadradic + cubic) * group, data = d)
)

fbs <- function(df, ix, .data, .formula) {
    df <- df[ix, ]
    m <- lm(.formula, data = df)
    n <- nrow(.data)
    k <- n / 2
    x <- predict(m, .data)
    x <- c(x, x[1:k] - x[(k+1):n])
}

f <- z_indegree_med ~ (linear + quadradic + cubic) * group
B <- boot(d, fbs, R = 10000, stype = "i", .data = newdata, .formula = f)
saveRDS(B, file = "bootstrap/median/boot-10000.rds")

b_ci <- with_progress({
    p <- progressor(nrow(newdata))
    map(1:nrow(newdata), function(i, b, conf) {
        boot.ci(b, conf = conf, type = "bca", index = i)$bca[4:5]
        p()
    }, b = B, conf = .95)
})
saveRDS(b_ci, file = "bootstrap/median/boot-10000_bs-bca_conf-95.rds")
