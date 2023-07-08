library(dplyr)
library(purrr)
library(boot)

d <- readRDS(file = "data/asd-na_netstats-ran.rds")
orth_poly <- poly(d$nproduced, 3)
d <- bind_cols(d, as_tibble(orth_poly)) %>%
    rename(linear = "1", quadradic = "2", cubic = "3")

x_tbl <- predict(orth_poly, seq(20, 580, by = 20))
x_tbl <- tibble(
    group = gl(3, nrow(x_tbl), labels = c(levels(d$group), "diff")),
    linear = rep(x_tbl[,1], 3),
    quadradic = rep(x_tbl[,2], 3),
    cubic = rep(x_tbl[,3], 3)
)

saveRDS(x_tbl, file = "bootstrap/x_tbl.rds")
